package comicgen

import (
	"bufio"
	"bytes"
	"encoding/json"
	"fmt"
	"image"
	"image/color"
	_ "image/jpeg" // For JPEG decoding
	"io/ioutil"
	"log"
	"math"
	"math/rand"
	"net/http"
	"os"
	"strconv"
	"strings"
	"sync"
	"unicode"
	"unicode/utf8"

	"github.com/golang/freetype/truetype"
	"github.com/llgcode/draw2d"
	"github.com/llgcode/draw2d/draw2dimg"
	"github.com/llgcode/draw2d/draw2dkit"
	"golang.org/x/image/draw"
	"golang.org/x/image/font"
	"golang.org/x/image/math/f64"
	"golang.org/x/image/math/fixed"
)

const arrowHeight float64 = 5

const (
	textAlignLeft int = iota
	textAlignCenter
	textAlignRight
)

type Emotion string

const (
	EmotionAngry    Emotion = "angry"
	EmotionBored            = "bored"
	EmotionHappy            = "happy"
	EmotionHi               = "hi"
	EmotionIdle             = "idle"
	EmotionLove             = "love"
	EmotionMe               = "me"
	EmotionQuestion         = "question"
	EmotionSad              = "sad"
	EmotionScared           = "scared"
	EmotionSmug             = "smug"
	EmotionWalk             = "walk"
	EmotionYell             = "yell"
	EmotionYou              = "you"
)

var emotionTriggers = map[Emotion][]string{
	EmotionAngry:    {"grr", "fuck", "damned", "goddamned", "shit", "angry", "upset", "😬", "😐", "😠", "😤", ">_<"},
	EmotionBored:    {"sigh", "boring", "bored", "...", "💤", "😴", ":|"},
	EmotionHappy:    {":)", "yay", "happy", "awesome", "😀", "😬", "😁", "😂", "😃", "😄", "😅", "😆", "😉", "😊", "🙂", "😋", "😌", "😜", "😝", "😛", "😏"},
	EmotionHi:       {"Hello", "Hi", "Yo", "Hey", "Heya"},
	EmotionLove:     {"<3", "❤️", "💛", "💙", "💜", "💔", "❣️", "💕", "💞", "💓", "💗", "💖", "💘", "💝", "💟"},
	EmotionMe:       {"me", "myself", "i'm", "i am", "im "},
	EmotionYou:      {"you", "you're", "you are", "youre"},
	EmotionQuestion: {"?", "what", "wat"},
	EmotionSad:      {":(", "sad", "😧", "😢", "😓", "😭", "😰"},
	EmotionScared:   {"no", "ack", "arg", "scared", "uh oh", "😱", "😳"},
	EmotionSmug:     {"of course", "obviously", "i know"},
	EmotionYell:     {"!"},
	EmotionWalk:     {"bye", "later"},
}

type Character struct {
	FileName string
	Name     string
	Width    int
	Height   int
	Frames   int
	Emotions map[Emotion][]int
}

func (c *Character) GetFrame(message string) int {
	possible := []Emotion{}
	for e, p := range emotionTriggers {
		for _, s := range p {
			if strings.Index(message, s) != -1 {
				possible = append(possible, e)
			}
		}
	}
	if len(possible) == 0 {
		possible = append(possible, EmotionIdle)
		if rand.Float64() > 0.7 {
			if rand.Float64() > 0.5 {
				possible = append(possible, EmotionHappy)
			} else {
				possible = append(possible, EmotionYell)
			}
		}
	}

	tot := 0
	for _, e := range possible {
		tot += len(c.Emotions[e])
	}

	if tot == 0 {
		return 1 + rand.Intn(c.MaxFrame()-1)
	}

	ran := rand.Intn(tot)
	cur := 0
	for _, e := range possible {
		l := len(c.Emotions[e])
		if cur+l > ran {
			return c.Emotions[e][ran-cur]
		}
		cur += l
	}

	return 1 + rand.Intn(c.MaxFrame()-1)
}

func (c *Character) MaxFrame() int {
	m := 1
	for _, r := range c.Emotions {
		for _, f := range r {
			if f > m {
				m = f
			}
		}
	}
	return m
}

// ComicGen is a comic generator!
type ComicGen struct {
	sync.Mutex
	font            *draw2d.FontData
	glyphBuf        *truetype.GlyphBuf
	img             *image.RGBA
	gc              *draw2dimg.GraphicContext
	avatars         map[int]image.Image
	emoji           map[rune]string
	room            image.Image
	characters      map[int]*Character
	characterImages map[int]image.Image
}

var googleEmoji = map[rune]string{}
var twitterEmoji = map[rune]string{}
var defaultAvatars = []string{}
var defaultCharacters = []*Character{}
var defaultRooms = []string{}

type ComicType int

const (
	ComicTypeSimple ComicType = iota
	ComicTypeChat
)

var simpleRenderers = []cellRenderer{
	&oneSpeakerCellRenderer{},
	&flippedOneSpeakerCellRenderer{},
	&oneSpeakerMonologueCellRenderer{},
	&twoSpeakerCellRenderer{},
}

var chatRenderers = []cellRenderer{
	&oneSpeakerChatCellRenderer{},
	&twoSpeakerChatCellRenderer{},
}

func parseEmoji(emoji map[rune]string, path string) {
	emojiFiles, err := ioutil.ReadDir(path)
	if err != nil {
		fmt.Errorf("Could not open emoji directory: %s %v", path, err)
	}

	for _, emojiFile := range emojiFiles {
		if emojiFile.IsDir() {
			continue
		}
		name := emojiFile.Name()
		if strings.HasSuffix(name, ".png") {
			name = name[:len(name)-4]
		} else {
			continue
		}
		if strings.HasPrefix(name, "emoji_u") {
			name = name[7:]
		}

		var chars []string
		if strings.Index(name, "-") != -1 {
			chars = strings.Split(name, "-")
		} else {
			chars = strings.Split(name, "_")
		}

		if len(chars) == 1 {
			i, e := strconv.ParseInt(chars[0], 16, 32)
			if e != nil {
				continue
			}

			emoji[rune(i)] = path + "/" + emojiFile.Name()
		}
	}
}

func firstUpper(s string) string {
	if s == "" {
		return ""
	}
	r, n := utf8.DecodeRuneInString(s)
	return string(unicode.ToUpper(r)) + s[n:]
}

func init() {
	var avatarFiles []os.FileInfo
	var err error
	if avatarFiles, err = ioutil.ReadDir("avatars"); err != nil {
		fmt.Errorf("Could not open avatars directory: %v", err)
	}

	for _, avatarFile := range avatarFiles {
		if avatarFile.IsDir() {
			continue
		}
		defaultAvatars = append(defaultAvatars, "avatars/"+avatarFile.Name())
	}

	parseEmoji(googleEmoji, "emoji/google")
	parseEmoji(twitterEmoji, "emoji/twitter")

	var roomFiles []os.FileInfo
	if roomFiles, err = ioutil.ReadDir("rooms"); err != nil {
		fmt.Errorf("Could not open rooms directory: %v", err)
	}

	for _, characterFile := range roomFiles {
		if characterFile.IsDir() {
			continue
		}
		name := characterFile.Name()
		defaultRooms = append(defaultRooms, "rooms/"+name)
	}

	var characterFiles []os.FileInfo
	if characterFiles, err = ioutil.ReadDir("characters"); err != nil {
		fmt.Errorf("Could not open characters directory: %v", err)
	}

	for _, characterFile := range characterFiles {
		if characterFile.IsDir() {
			continue
		}
		name := characterFile.Name()
		if strings.HasSuffix(name, ".png") {
			n := name[:len(name)-4]
			defaultCharacters = append(defaultCharacters, &Character{
				FileName: "characters/" + name,
				Name:     firstUpper(n),
				Width:    161,
				Height:   230,
				Frames:   12,
				Emotions: loadEmotions("characters/" + n + ".json"),
			})
		}
	}

	draw2d.SetFontFolder("fonts")
}

func loadEmotions(path string) map[Emotion][]int {
	b, err := ioutil.ReadFile(path)
	if err != nil {
		return map[Emotion][]int{}
	}

	var e map[Emotion][]int

	if err := json.Unmarshal(b, &e); err != nil {
		return map[Emotion][]int{}
	}

	return e
}

// NewComicGen creates a new comic generator.
func NewComicGen(font string, useGooglEmoji bool) *ComicGen {
	emoji := googleEmoji
	if !useGooglEmoji {
		emoji = twitterEmoji
	}
	return &ComicGen{
		font:            &draw2d.FontData{font, draw2d.FontFamilySans, draw2d.FontStyleNormal},
		glyphBuf:        &truetype.GlyphBuf{},
		avatars:         map[int]image.Image{},
		emoji:           emoji,
		characters:      map[int]*Character{},
		characterImages: map[int]image.Image{},
	}
}

// Message contains a single message for a comment.
type Message struct {
	Speaker int
	Text    string
	Author  string
}

// Script contains all the data necessary to generate a comic.
type Script struct {
	Type     ComicType
	Messages []*Message
	Avatars  map[int]string
	Author   string
	Room     string
}

func renderersForType(ct ComicType) []cellRenderer {
	switch ct {
	case ComicTypeChat:
		return chatRenderers
	}
	return simpleRenderers
}

type FinishedFunc func([]cellRenderer) bool

func finishedFuncForType(ct ComicType) FinishedFunc {
	switch ct {
	case ComicTypeChat:
		return FinishedFunc(func(currentPlan []cellRenderer) bool {
			return (len(currentPlan) < 4 || len(currentPlan)%3 == 2)
		})
	}
	return FinishedFunc(func(currentPlan []cellRenderer) bool {
		return len(currentPlan) < 4 || len(currentPlan)%3 == 0
	})
}

// MakeComic makes a comic.
func (comic *ComicGen) MakeComic(script *Script) (image.Image, error) {
	messages := script.Messages

	renderers := renderersForType(script.Type)

	// Create all plans that are sufficient, and pick a random one.
	plans := [][]cellRenderer{}
	allplans := [][]cellRenderer{}

	ff := finishedFuncForType(script.Type)

	planchan := make(chan []cellRenderer, len(messages)*len(messages))
	go createPlans(planchan, renderers, make([]cellRenderer, 0), messages, 0)
	for {
		plan := <-planchan
		if plan == nil {
			break
		}
		if ff(plan) {
			plans = append(plans, plan)
		}
		allplans = append(allplans, plan)
	}

	if len(plans) == 0 && len(allplans) == 0 {
		return nil, fmt.Errorf("Too much text for such a small comic.")
	}
	var plan []cellRenderer
	if len(plans) > 0 {
		plan = plans[rand.Intn(len(plans))]
	} else {
		plan = allplans[rand.Intn(len(allplans))]
		for !ff(plan) {
			plan = plan[1:]
		}
		if len(plan) == 0 {
			return nil, fmt.Errorf("Too much text for such a small comic.")
		}
	}

	cellWidth := 200
	cellHeight := 200
	cellBorder := 20
	switch script.Type {
	case ComicTypeChat:
		cellWidth = 220
		cellHeight = 220
		cellBorder = 5
	}

	planWidth := len(plan)
	if planWidth > 4 {
		planWidth = 3
	}
	planHeight := int(math.Ceil(float64(len(plan)) / float64(planWidth)))

	if len(plan) < 5 {
		switch script.Type {
		case ComicTypeChat:
			switch len(plan) {
			case 1:
				planWidth = 2
				planHeight = 1
			case 2:
				planWidth = 3
				planHeight = 1
			case 3:
				planWidth = 2
				planHeight = 2
			}
		}
	}

	extraHeight := 0
	if script.Type == ComicTypeSimple {
		extraHeight = 15
	}

	width := planWidth*(cellWidth+cellBorder) - cellBorder + 10
	height := planHeight*(cellHeight+cellBorder) - cellBorder + 10 + extraHeight

	// Initialize the context.
	comic.img = image.NewRGBA(image.Rect(0, 0, width, height))
	draw.Draw(comic.img, comic.img.Bounds(), image.White, image.ZP, draw.Src)

	comic.gc = draw2dimg.NewGraphicContext(comic.img)
	comic.gc.SetDPI(72)
	comic.gc.SetFontData(*comic.font)
	comic.gc.SetFont(draw2d.GetFont(*comic.font))

	for i, url := range script.Avatars {
		if url != "" {
			image, err := fetchAvatar(url)
			if err == nil {
				comic.avatars[i] = image
			}
		}
	}

	var seen map[int]bool

	if script.Type != ComicTypeChat {
		seen = map[int]bool{}
		for _, m := range script.Messages {
			if comic.avatars[m.Speaker] == nil {
				for {
					i := rand.Intn(len(defaultAvatars))
					if !seen[i] {
						seen[i] = true
						if img, err := loadImage(defaultAvatars[i]); err == nil {
							comic.avatars[m.Speaker] = img
						}
						break
					}
				}
			}
		}
	}

	comic.room, _ = loadImage(defaultRooms[rand.Intn(len(defaultRooms))])

	seen = map[int]bool{}
	for _, m := range script.Messages {
		if comic.characterImages[m.Speaker] == nil {
			for {
				i := rand.Intn(len(defaultCharacters))
				if !seen[i] {
					seen[i] = true
					comic.characters[m.Speaker] = defaultCharacters[i]
					if img, err := loadImage(defaultCharacters[i].FileName); err == nil {
						comic.characterImages[m.Speaker] = img
					}
					break
				}
			}
		}
	}

	offset := 0
	if script.Type == ComicTypeChat {
		comic.drawIntro(script, 5, 5, float64(cellWidth), float64(cellHeight))
		offset = 1
	}

	for i, c := 0, 0; i < len(plan); i++ {
		renderer := plan[i]
		renderer.render(comic, messages[c:c+renderer.lines()], 5+float64(cellWidth+cellBorder)*float64((i+offset)%planWidth), 5+float64(cellHeight+cellBorder)*float64((i+offset)/planWidth), float64(cellWidth), float64(cellHeight))
		c += renderer.lines()
	}
	if script.Type == ComicTypeSimple {
		comic.drawTextInRect(comic.gc, color.RGBA{0xdd, 0xdd, 0xdd, 0xff}, textAlignRight, 1, fmt.Sprintf("A comic by %v.", script.Author), 3, 0, float64(height)-20, float64(width), 20)
	}

	return comic.img, nil
}

func loadImage(path string) (image.Image, error) {
	file, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	img, _, err := image.Decode(bufio.NewReader(file))
	if err != nil {
		return nil, err
	}

	return img, nil
}

func fetchAvatar(url string) (image.Image, error) {
	resp, err := http.Get(url)
	if err != nil {
		return nil, err
	}

	defer resp.Body.Close()

	image, _, err := image.Decode(resp.Body)
	if err != nil {
		return nil, err
	}

	return image, nil
}

func countSpeakers(messages []*Message) int {
	seenMap := make(map[int]bool)
	for _, message := range messages {
		seenMap[message.Speaker] = true
	}
	return len(seenMap)
}

func createPlans(planchan chan []cellRenderer, renderers []cellRenderer, currentPlan []cellRenderer, remainingScript []*Message, depth int) {
	newPlanchan := make(chan []cellRenderer, len(remainingScript)*len(remainingScript))
	count := 0
	for _, renderer := range renderers {
		if renderer.lines() <= len(remainingScript) {
			if s := renderer.satisfies(remainingScript); s != 0 {
				plan := append(append([]cellRenderer{}, currentPlan...), renderer)
				if len(remainingScript)-s == 0 {
					planchan <- plan
					count++
				} else {
					go createPlans(newPlanchan, renderers, plan, remainingScript[s:], depth+1)
				}
			} else {
				count++
			}
		} else {
			count++
		}
	}
	for count < len(renderers) {
		plan := <-newPlanchan
		if plan == nil {
			count++
		} else {
			planchan <- plan
		}
	}
	planchan <- nil
}

func drawSpeech(gc *draw2dimg.GraphicContext, border, radius, x, y, width, height, pointX, pointY float64) {
	gc.Save()
	defer gc.Restore()

	bubble := &draw2d.Path{}

	bubble.MoveTo(x+radius, y)
	bubble.LineTo(x+width-radius, y)
	// top right corner
	bubble.QuadCurveTo(x+width, y, x+width, y+radius)
	bubble.LineTo(x+width, y+height-radius)
	// botttom right corner
	bubble.QuadCurveTo(x+width, y+height, x+width-radius, y+height)
	bubble.LineTo(x+radius, y+height)
	// bottom left corner
	bubble.QuadCurveTo(x, y+height, x, y+height-radius)
	bubble.LineTo(x, y+radius)
	// top left corner
	bubble.QuadCurveTo(x, y, x+radius, y)
	bubble.Close()

	arrow := &draw2d.Path{}

	cx := x + width/2
	cy := y + height/2

	dx := pointX - cx
	dy := pointY - cy

	d := float64(math.Sqrt(dx*dx + dy*dy))

	nx := dx / d
	ny := dy / d

	var r float64
	if width > height {
		r = height / 2
	} else {
		r = width / 2
	}
	r *= 0.8

	sx := cx + r*nx
	sy := cy + r*ny

	arrowWidth := math.Min(d*0.2, 4)

	arrow.MoveTo(pointX, pointY)
	arrow.LineTo(sx+ny*arrowWidth, sy+-nx*arrowWidth)
	arrow.LineTo(sx+-ny*arrowWidth, sy+nx*arrowWidth)
	arrow.LineTo(pointX, pointY)
	arrow.Close()

	//Draw the black
	gc.SetLineCap(draw2d.RoundCap)
	gc.SetLineJoin(draw2d.RoundJoin)
	gc.SetLineWidth(border * 2)
	gc.SetStrokeColor(color.Black)
	gc.SetFillColor(color.Black)
	gc.FillStroke(bubble)
	gc.FillStroke(arrow)

	// Draw the white
	gc.SetFillColor(color.White)
	gc.Fill(bubble)
	gc.Fill(arrow)
}

func (comic *ComicGen) loadGlyph(gc *draw2dimg.GraphicContext, glyph truetype.Index) error {
	return comic.glyphBuf.Load(gc.Current.Font, fixed.Int26_6(gc.Current.Scale), glyph, font.HintingNone)
}

func (comic *ComicGen) drawGlyph(gc *draw2dimg.GraphicContext, glyph truetype.Index, dx, dy float64) error {
	if err := comic.loadGlyph(gc, glyph); err != nil {
		return err
	}
	e0 := 0
	for _, e1 := range comic.glyphBuf.Ends {
		draw2dimg.DrawContour(gc, comic.glyphBuf.Points[e0:e1], dx, dy)
		e0 = e1
	}
	return nil
}

func (comic *ComicGen) drawEmoji(gc *draw2dimg.GraphicContext, r rune, x, y, width, height float64) error {
	if file, err := os.Open(comic.emoji[r]); err == nil {
		defer file.Close()
		if emoji, _, err := image.Decode(bufio.NewReader(file)); err == nil {
			gc.Save()
			gc.ComposeMatrixTransform(draw2d.NewTranslationMatrix(x, y))
			comic.drawImage(gc, emoji, image.Rectangle{image.Point{0, 0}, image.Point{int(width), int(height)}})
			gc.Restore()
			return nil
		} else {
			return err
		}
	} else {
		return err
	}
}

func (comic *ComicGen) createStringPath(gc *draw2dimg.GraphicContext, s string, x, y float64) float64 {
	font := gc.Current.Font
	startx := x
	prev, hasPrev := truetype.Index(0), false
	for _, r := range s {
		index := font.Index(r)
		if hasPrev {
			x += fUnitsToFloat64(font.Kern(fixed.Int26_6(gc.Current.Scale), prev, index))
		}

		if comic.emoji[r] != "" {
			l, t, ri, b := comic.getStringBounds(gc, string(r))
			l -= 1
			t -= 1
			ri += 1
			b += 1
			comic.drawEmoji(gc, r, x+l, y+t, ri-l, b-t)
		} else {
			err := comic.drawGlyph(gc, index, x, y)
			if err != nil {
				return startx - x
			}
		}
		x += fUnitsToFloat64(font.HMetric(fixed.Int26_6(gc.Current.Scale), index).AdvanceWidth)
		prev, hasPrev = index, true
	}
	return x - startx
}

func (comic *ComicGen) getStringBounds(gc *draw2dimg.GraphicContext, s string) (left, top, right, bottom float64) {
	font := gc.Current.Font
	top, left, bottom, right = 10e6, 10e6, -10e6, -10e6
	cursor := 0.0
	prev, hasPrev := truetype.Index(0), false
	for _, rune := range s {
		index := font.Index(rune)
		if hasPrev {
			cursor += fUnitsToFloat64(font.Kern(fixed.Int26_6(gc.Current.Scale), prev, index))
		}

		if err := comic.loadGlyph(gc, index); err != nil {
			log.Println(err)
			return 0, 0, 0, 0
		}
		e0 := 0
		for _, e1 := range comic.glyphBuf.Ends {
			ps := comic.glyphBuf.Points[e0:e1]
			for _, p := range ps {
				x, y := pointToF64Point(p)
				top = math.Min(top, y)
				bottom = math.Max(bottom, y)
				left = math.Min(left, x+cursor)
				right = math.Max(right, x+cursor)
			}
		}
		cursor += fUnitsToFloat64(font.HMetric(fixed.Int26_6(gc.Current.Scale), index).AdvanceWidth)
		prev, hasPrev = index, true
	}
	return left, top, right, bottom
}

func (comic *ComicGen) drawTextInRect(gc *draw2dimg.GraphicContext, color color.Color, align int, spacing float64, text string, border, x, y, width, height float64) {
	wrapText, fontSize, _, _ := comic.fitText(gc, spacing, text, width-border*2, height-border*2)

	if fontSize > 40 {
		fontSize = 40
	}

	comic.drawText(gc, color, fontSize, align, spacing, wrapText, x+border, y, width-border*2, height)
}

func (comic *ComicGen) drawText(gc *draw2dimg.GraphicContext, color color.Color, fontSize float64, align int, spacing float64, text string, x, y, width, height float64) (float64, float64) {
	gc.Save()
	defer gc.Restore()

	gc.SetStrokeColor(color)
	gc.SetFillColor(color)
	gc.SetFontSize(fontSize)

	l, t, r, b := comic.textBounds(gc, spacing, text)

	textHeight := -t + b

	top := -t + y
	if height != 0 {
		top += (height - textHeight) / 2
	}

	lineHeight := fUnitsToFloat64(gc.Current.Font.VMetric(fixed.Int26_6(gc.Current.Scale), 0).AdvanceHeight) * spacing

	// Draw the text.
	lines := strings.Split(text, "\n")
	for i, line := range lines {
		l, _, r, _ := comic.textBounds(gc, spacing, line)
		textWidth := -l + r
		var px float64
		switch align {
		case textAlignLeft:
			px = x - l
		case textAlignCenter:
			px = x - l + (width-textWidth)/2
		case textAlignRight:
			px = width - textWidth
		}
		py := top + lineHeight*float64(i)

		comic.createStringPath(gc, line, px, py)
		gc.Fill()
	}

	return -l + r, -t + b
}

func fUnitsToFloat64(x fixed.Int26_6) float64 {
	scaled := x << 2
	return float64(scaled/256) + float64(scaled%256)/256.0
}

func pointToF64Point(p truetype.Point) (x, y float64) {
	return fUnitsToFloat64(p.X), -fUnitsToFloat64(p.Y)
}

func (comic *ComicGen) textBounds(gc *draw2dimg.GraphicContext, spacing float64, text string) (left, top, right, bottom float64) {
	lines := strings.Split(text, "\n")

	lineHeight := fUnitsToFloat64(gc.Current.Font.VMetric(fixed.Int26_6(gc.Current.Scale), 0).AdvanceHeight) * spacing

	for i, line := range lines {
		l, t, r, b := comic.getStringBounds(gc, line)

		if l < left {
			left = l
		}
		if r > right {
			right = r
		}
		if t < top {
			top = t
		}

		b += lineHeight * float64(i) * spacing
		if b > bottom {
			bottom = b
		}
	}
	return
}

func (comic *ComicGen) textSize(gc *draw2dimg.GraphicContext, spacing float64, text string) (width, height float64) {
	left, top, right, bottom := comic.textBounds(gc, spacing, text)
	return -left + right, -top + bottom
}

func (comic *ComicGen) fitText(gc *draw2dimg.GraphicContext, spacing float64, text string, width, height float64) (wrappedText string, fontSize, wrapWidth, wrapHeight float64) {
	if text == "" {
		return text, 1, 0, 0
	}
	gc.Save()
	defer gc.Restore()

	// Match aspect ratios, favoring width.
	aspect := width / height
	for low, high := 1.0, 100.0; high > low+0.1; {
		fontSize = low + (high-low)/2

		gc.SetFontSize(fontSize)

		wrappedText, _ = comic.wrapText(gc, spacing, text, width)
		wrapWidth, wrapHeight = comic.textSize(gc, spacing, wrappedText)
		newTextAspect := wrapWidth / wrapHeight
		if newTextAspect > aspect {
			low = fontSize
		} else if newTextAspect < aspect {
			high = fontSize
		}
	}
	// Scale the contents to fit the window (as its possible the font size is too large, but satisfies the aspect ratio better)
	scale := width / wrapWidth
	if wrapHeight*scale > height {
		scale = height / wrapHeight
	}
	fontSize *= scale
	wrapWidth *= scale
	wrapHeight *= scale

	return
}

func (comic *ComicGen) fitTextHeight(gc *draw2dimg.GraphicContext, fs float64, spacing float64, text string, width, height float64) (wrappedText string, fontSize, wrapWidth, wrapHeight float64) {
	gc.Save()
	defer gc.Restore()

	fontSize = fs

	for {
		gc.SetFontSize(fontSize)

		wrappedText, _ = comic.wrapText(gc, spacing, text, width)
		wrapWidth, wrapHeight = comic.textSize(gc, spacing, wrappedText)

		if wrapHeight < height && wrapWidth < width {
			break
		}
		fontSize--
	}
	return
}

func (comic *ComicGen) wrapText(gc *draw2dimg.GraphicContext, spacing float64, text string, wrapWidth float64) (string, float64) {
	var buffer bytes.Buffer
	var maxWidth float64
	lines := strings.Split(text, "\n")
	for i, line := range lines {
		width := comic.wrapLine(&buffer, gc, spacing, line, wrapWidth)
		if width > maxWidth {
			maxWidth = width
		}
		if i < len(lines)-1 {
			buffer.WriteString("\n")
		}
	}
	return buffer.String(), maxWidth
}

func (comic *ComicGen) wrapLine(buffer *bytes.Buffer, gc *draw2dimg.GraphicContext, spacing float64, line string, wrapWidth float64) float64 {
	var width float64
	var runningWidth float64
	var maxWidth float64
	words := strings.Split(line, " ")
	for i, word := range words {
		if i != 0 {
			width, _ = comic.textSize(gc, spacing, " "+word)
		} else {
			width, _ = comic.textSize(gc, spacing, word)
		}
		if width > maxWidth {
			maxWidth = width
		}
		runningWidth += width
		if runningWidth >= wrapWidth && i != 0 {
			runningWidth = width
			buffer.WriteString("\n")
		} else if i != 0 {
			buffer.WriteString(" ")
		}
		buffer.WriteString(word)
	}
	return maxWidth
}

func insetRectangle(x, y, width, height, inset float64) (float64, float64, float64, float64) {
	return insetRectangleHV(x, y, width, height, inset, inset)
}

func insetRectangleHV(x, y, width, height, horizontal, vertical float64) (float64, float64, float64, float64) {
	return insetRectangleLRTB(x, y, width, height, horizontal, horizontal, vertical, vertical)
}

func insetRectangleLRTB(x, y, width, height, left, right, top, bottom float64) (float64, float64, float64, float64) {
	return x + left, y + top, width - left - right, height - top - bottom
}

func rect(gc *draw2dimg.GraphicContext, x, y, width, height float64) {
	gc.MoveTo(x, y)
	gc.LineTo(x+width, y)
	gc.LineTo(x+width, y+height)
	gc.LineTo(x, y+height)
	gc.LineTo(x, y)
}

var outlineColor = color.RGBA{0x00, 0x00, 0x00, 0x22}

func outline(gc *draw2dimg.GraphicContext, color color.Color, x, y, width, height, strokewidth float64) {
	gc.Save()
	defer gc.Restore()

	gc.SetLineCap(draw2d.ButtCap)
	gc.SetLineJoin(draw2d.MiterJoin)
	gc.SetLineWidth(strokewidth)
	gc.SetStrokeColor(color)
	rect(gc, x, y, width, height)
	gc.Stroke()
}

type cellRenderer interface {
	// Returns the maximum number of lines this renderer can satisfy.
	lines() int
	// If this returns a > 0 value, this renderer has said to be able to satisfy that many lines of the script.
	// If this returns 0, this renderer cannot satisfy any lines and is unusable.
	satisfies(messages []*Message) int
	render(comic *ComicGen, messages []*Message, x, y, width, height float64)
}

type oneSpeakerCellRenderer struct{}

func (c *oneSpeakerCellRenderer) lines() int {
	return 1
}

func (c *oneSpeakerCellRenderer) satisfies(messages []*Message) int {
	return 1
}

func (c *oneSpeakerCellRenderer) render(comic *ComicGen, messages []*Message, x, y, width, height float64) {
	gc := comic.gc
	avatars := comic.avatars

	outline(gc, outlineColor, x, y, width, height, 2)

	if len(messages) != c.lines() {
		return
	}

	border := float64(5)
	bounds := image.Rectangle{image.Point{0, 0}, image.Point{88, 88}}

	avatar := avatars[messages[0].Speaker]

	gc.Save()
	gc.ComposeMatrixTransform(draw2d.NewTranslationMatrix(x+border, y+height-border-float64(bounds.Dy())))
	comic.drawImage(gc, avatar, bounds)
	outline(gc, outlineColor, 0, 0, float64(bounds.Dx()), float64(bounds.Dy()), 2)
	gc.Restore()

	bX, bY, bWidth, bHeight := insetRectangleLRTB(x, y, width, height, border, border, border, border+float64(bounds.Dy())+arrowHeight*2)

	drawSpeech(gc, 2, border, bX, bY, bWidth, bHeight, bX+rand.Float64()*float64(bounds.Dx()), bY+bHeight+arrowHeight)
	comic.drawTextInRect(gc, image.Black, textAlignCenter, 1, string(messages[0].Text), arrowHeight, bX, bY, bWidth, bHeight)
}

type flippedOneSpeakerCellRenderer struct{}

func (c *flippedOneSpeakerCellRenderer) lines() int {
	return 1
}

func (c *flippedOneSpeakerCellRenderer) satisfies(messages []*Message) int {
	return 1
}

func (c *flippedOneSpeakerCellRenderer) render(comic *ComicGen, messages []*Message, x, y, width, height float64) {
	gc := comic.gc
	avatars := comic.avatars

	outline(gc, outlineColor, x, y, width, height, 2)

	if len(messages) != c.lines() {
		return
	}

	border := float64(5)
	bounds := image.Rectangle{image.Point{0, 0}, image.Point{88, 88}}

	avatar := avatars[messages[0].Speaker]

	gc.Save()
	gc.ComposeMatrixTransform(draw2d.NewTranslationMatrix(x+border, y+border))
	comic.drawImage(gc, avatar, bounds)
	outline(gc, outlineColor, 0, 0, float64(bounds.Dx()), float64(bounds.Dy()), 2)
	gc.Restore()

	bX, bY, bWidth, bHeight := insetRectangleLRTB(x, y, width, height, border, border, border+float64(bounds.Dy())+arrowHeight*2, border)

	drawSpeech(gc, 2, border, bX, bY, bWidth, bHeight, bX+rand.Float64()*float64(bounds.Dx()), bY-arrowHeight)
	comic.drawTextInRect(gc, image.Black, textAlignCenter, 1, string(messages[0].Text), arrowHeight, bX, bY, bWidth, bHeight)
}

type twoSpeakerCellRenderer struct{}

func (c *twoSpeakerCellRenderer) lines() int {
	return 2
}

func (c *twoSpeakerCellRenderer) satisfies(messages []*Message) int {
	if len(messages) > 1 && countSpeakers(messages[:c.lines()]) == 2 && len(messages[0].Text) < 128 && len(messages[1].Text) < 128 {
		return 2
	}
	return 0
}

func (c *twoSpeakerCellRenderer) render(comic *ComicGen, messages []*Message, x, y, width, height float64) {
	gc := comic.gc
	avatars := comic.avatars

	outline(gc, outlineColor, x, y, width, height, 2)

	if len(messages) != c.lines() {
		return
	}

	border := float64(5)
	bounds := image.Rectangle{image.Point{0, 0}, image.Point{88, 88}}
	flipped := rand.Float64() >= 0.5
	// get a rectangle for half the area
	aX, aY, aWidth, aHeight := insetRectangleLRTB(x, y, width, height, 0, 0, 0, height/2)
	for i := 0; i < 2; i++ {

		avatar := avatars[messages[i].Speaker]

		gc.Save()
		if flipped {
			gc.ComposeMatrixTransform(draw2d.NewTranslationMatrix(aX+aWidth-border-float64(bounds.Dx()), aY+aHeight-border-float64(bounds.Dy())))
		} else {
			gc.ComposeMatrixTransform(draw2d.NewTranslationMatrix(aX+border, aY+border))
		}
		comic.drawImage(gc, avatar, bounds)
		outline(gc, outlineColor, 0, 0, float64(bounds.Dx()), float64(bounds.Dy()), 2)
		gc.Restore()

		bX, bY, bWidth, bHeight := insetRectangleLRTB(aX, aY, aWidth, aHeight, border, border+float64(bounds.Dx())+arrowHeight*3, border, border)

		if !flipped {
			bX += aWidth - bWidth - (bX - x) - border
		}

		arrowX := -arrowHeight * 2
		if flipped {
			arrowX = bWidth + arrowHeight*2
		}

		drawSpeech(gc, 2, border, bX, bY, bWidth, bHeight, bX+arrowX, bY+rand.Float64()*float64(bounds.Dx()))
		comic.drawTextInRect(gc, image.Black, textAlignCenter, 1, string(messages[i].Text), 10, bX, bY, bWidth, bHeight)

		flipped = !flipped
		aY += aHeight
	}
}

type oneSpeakerMonologueCellRenderer struct{}

func (c *oneSpeakerMonologueCellRenderer) lines() int {
	return 2
}

func (c *oneSpeakerMonologueCellRenderer) satisfies(messages []*Message) int {
	if len(messages) > 1 && countSpeakers(messages[:c.lines()]) == 1 && len(messages[0].Text) < 128 {
		return 2
	}
	return 0
}

func (c *oneSpeakerMonologueCellRenderer) render(comic *ComicGen, messages []*Message, x, y, width, height float64) {
	gc := comic.gc
	avatars := comic.avatars

	outline(gc, outlineColor, x, y, width, height, 2)

	if len(messages) != c.lines() {
		return
	}

	border := float64(5)
	bounds := image.Rectangle{image.Point{0, 0}, image.Point{88, 88}}

	avatar := avatars[messages[0].Speaker]

	gc.Save()
	gc.ComposeMatrixTransform(draw2d.NewTranslationMatrix(x+border, y+height-border-float64(bounds.Dy())))
	comic.drawImage(gc, avatar, bounds)
	outline(gc, outlineColor, 0, 0, float64(bounds.Dx()), float64(bounds.Dy()), 2)
	gc.Restore()

	bX, bY, bWidth, bHeight := insetRectangleLRTB(x, y, width, height, border, border, border, border+float64(bounds.Dy())+arrowHeight*2)

	drawSpeech(gc, 2, border, bX, bY, bWidth, bHeight, bX+rand.Float64()*float64(bounds.Dx()), bY+bHeight+arrowHeight)
	comic.drawTextInRect(gc, image.Black, textAlignCenter, 1, string(messages[0].Text), arrowHeight, bX, bY, bWidth, bHeight)

	bX, bY, bWidth, bHeight = insetRectangleLRTB(x, y, width, height, border+float64(bounds.Dx())+arrowHeight*3, border, y+height-border*2-float64(bounds.Dy()), border)

	drawSpeech(gc, 2, border, bX, bY, bWidth, bHeight, bX-arrowHeight*2, bY+rand.Float64()*float64(bounds.Dy()))
	comic.drawTextInRect(gc, image.Black, textAlignCenter, 1, string(messages[1].Text), arrowHeight, bX, bY, bWidth, bHeight)
}

func (comic *ComicGen) drawImage(gc *draw2dimg.GraphicContext, img image.Image, bounds image.Rectangle) {
	if img == nil {
		return
	}
	ib := img.Bounds()
	gc.Save()
	defer gc.Restore()

	gc.ComposeMatrixTransform(draw2d.NewScaleMatrix(float64(bounds.Dx())/float64(ib.Dx()), float64(bounds.Dy())/float64(ib.Dy())))
	gc.DrawImage(img)
}

type oneSpeakerChatCellRenderer struct{}

func (c *oneSpeakerChatCellRenderer) lines() int {
	return 1
}

func (c *oneSpeakerChatCellRenderer) satisfies(messages []*Message) int {
	return 1
}

const chatBorder = 8

func (c *ComicGen) drawCharacter(sub *image.RGBA, message *Message, zoom float64, width, height, position float64, flip float64) {
	characterimg := c.characterImages[message.Speaker]
	character := c.characters[message.Speaker]

	frame := character.GetFrame(message.Text)

	b := characterimg.Bounds()

	ox := (character.Width * frame) % b.Dx()
	oy := ((character.Width * frame) / b.Dx()) * character.Height

	tr := draw2d.NewIdentityMatrix()
	tr.Compose(c.gc.Current.Tr)

	mx := width - float64(character.Width)*zoom/2.0

	tr.Compose(draw2d.NewTranslationMatrix(width*position+mx/2.0, height/2.0-10.0))
	tr.Compose(draw2d.NewScaleMatrix(flip*zoom/2.0, zoom/2.0))
	tr.Compose(draw2d.NewTranslationMatrix(float64(-ox), float64(-oy)))
	if flip == -1 {
		tr.Compose(draw2d.NewTranslationMatrix(-float64(character.Width), 0))
	}

	draw.CatmullRom.Transform(sub, f64.Aff3{tr[0], tr[1], tr[4], tr[2], tr[3], tr[5]}, characterimg, image.Rect(ox, oy, ox+character.Width, oy+character.Height), draw.Over, nil)
}

func (c *ComicGen) drawComicSpeech(message *Message, x, y, width, height, arrowx float64) float64 {
	c.gc.Save()
	defer c.gc.Restore()

	fontSize := 14.0
	c.gc.SetFontSize(fontSize)

	text, _ := c.wrapText(c.gc, 1, message.Text, width-12)

	w, h := c.textSize(c.gc, 1, text)

	if y+h > height-chatBorder*2 || w > width-12 {
		text, fontSize, w, h = c.fitTextHeight(c.gc, 14, 1, message.Text, width-chatBorder*2, height-chatBorder*2)
	}

	ox := rand.Float64() * (width - (w + chatBorder)) * 0.5
	drawSpeech(c.gc, 1, 4, x+ox, y, w+chatBorder, h+chatBorder-2, arrowx, height)
	_, h = c.drawText(c.gc, color.Black, fontSize, textAlignLeft, 1, text, x+ox+3, y+3, width-chatBorder, 0)

	return y + h + chatBorder
}

func (c *ComicGen) drawComic(messages []*Message, x, y, width, height float64) {
	c.gc.Save()
	defer c.gc.Restore()

	tr := draw2d.NewTranslationMatrix(x, y)
	c.gc.ComposeMatrixTransform(tr)

	if len(messages) < 1 || len(messages) > 2 {
		return
	}

	zoom := 1.0 + rand.Float64()

	// Draw background
	rb := c.room.Bounds()
	tr.Compose(draw2d.NewScaleMatrix(width/float64(rb.Dx()), height/float64(rb.Dy())))
	tr.Compose(draw2d.NewTranslationMatrix(0, -(height*zoom-height)*0.5))
	tr.Compose(draw2d.NewScaleMatrix(zoom, zoom))

	sub := c.img.SubImage(image.Rect(int(x), int(y), int(x+width), int(y+height))).(*image.RGBA)
	draw.BiLinear.Transform(sub, f64.Aff3{tr[0], tr[1], tr[4], tr[2], tr[3], tr[5]}, c.room, rb, draw.Over, nil)

	top := height/2.0 - 6

	if len(messages) == 1 {
		c.drawComicSpeech(messages[0], chatBorder, chatBorder, width-chatBorder*2, top, width/2)
		c.drawCharacter(sub, messages[0], zoom, width, height, 0, 1)
	} else {
		c.drawComicSpeech(messages[1], width*0.25, c.drawComicSpeech(messages[0], chatBorder, chatBorder, width-chatBorder*2, top, width*0.25), width*0.75-chatBorder*2, top, width*0.75)
		c.drawCharacter(sub, messages[0], zoom, width/2.0, height, 0, 1)
		c.drawCharacter(sub, messages[1], zoom, width/2.0, height, 1, -1)
	}

	outline(c.gc, color.Black, 2, 2, width-4, height-4, 4)
}

func (c *oneSpeakerChatCellRenderer) render(comic *ComicGen, messages []*Message, x, y, width, height float64) {
	comic.drawComic(messages, x, y, width, height)
}

type twoSpeakerChatCellRenderer struct{}

func (c *twoSpeakerChatCellRenderer) lines() int {
	return 2
}

func (c *twoSpeakerChatCellRenderer) satisfies(messages []*Message) int {
	if len(messages) > 1 && countSpeakers(messages[:c.lines()]) == 2 && len(messages[0].Text) < 128 && len(messages[1].Text) < 128 {
		return 2
	}
	return 0
}

func (c *twoSpeakerChatCellRenderer) render(comic *ComicGen, messages []*Message, x, y, width, height float64) {
	comic.drawComic(messages, x, y, width, height)
}

func (c *ComicGen) drawIntroCharacter(id int, name string, width, y float64) {
	c.gc.Save()
	defer c.gc.Restore()

	if name == "" {
		name = c.characters[id].Name
	}

	characterimg := c.characterImages[id]

	wrapText, fontSize, w, h := c.fitTextHeight(c.gc, 16, 1, name, width-76, 15)

	hasAvatar := c.avatars[id] != nil

	totalwidth := w + 32
	if hasAvatar {
		totalwidth += 28
	}

	ox := (width - totalwidth) / 2

	c.gc.ComposeMatrixTransform(draw2d.NewTranslationMatrix(ox, y))

	c.gc.Save()
	c.gc.ComposeMatrixTransform(draw2d.NewScaleMatrix(0.75, 0.75))
	tr := c.gc.Current.Tr
	draw.CatmullRom.Transform(c.img, f64.Aff3{tr[0], tr[1], tr[4], tr[2], tr[3], tr[5]}, characterimg, image.Rect(3, 3, 40, 40), draw.Over, nil)
	c.gc.Restore()

	c.gc.ComposeMatrixTransform(draw2d.NewTranslationMatrix(32, 0))

	if hasAvatar {
		c.gc.Save()
		c.gc.ComposeMatrixTransform(draw2d.NewTranslationMatrix(0, 3))
		c.drawImage(c.gc, c.avatars[id], image.Rectangle{image.Point{0, 0}, image.Point{26, 26}})

		c.gc.SetFillColor(color.White)
		draw2dkit.Rectangle(c.gc, -1, -1, 28, 28)
		draw2dkit.Circle(c.gc, 13, 13, 13)
		c.gc.Fill()

		c.gc.Restore()
		c.gc.ComposeMatrixTransform(draw2d.NewTranslationMatrix(28, 0))
	}

	c.drawText(c.gc, color.Black, fontSize, textAlignLeft, 1, wrapText, 0, (35-h)/2, w, h)
}

func (c *ComicGen) drawIntro(script *Script, x, y, width, height float64) {
	c.gc.Save()
	defer c.gc.Restore()

	charcount := 0.0
	seen := map[int]bool{}
	for _, m := range script.Messages {
		if !seen[m.Speaker] {
			charcount++
			seen[m.Speaker] = true
		}
		if len(seen) > 5 {
			break
		}
	}

	oy := (200 - (charcount*30.0 + 20.0)) / 2

	c.gc.Save()
	c.gc.ComposeMatrixTransform(draw2d.NewTranslationMatrix(x, y+oy))
	c.drawText(c.gc, color.Black, 16, textAlignCenter, 1, "A comic starring:", 0, 0, width, 20)

	iy := 20.0

	seen = map[int]bool{}
	for _, m := range script.Messages {
		if !seen[m.Speaker] {
			c.drawIntroCharacter(m.Speaker, m.Author, width, iy)
			iy += 30
			seen[m.Speaker] = true
		}
		if len(seen) > 5 {
			break
		}
	}
	c.gc.Restore()
	c.drawTextInRect(c.gc, color.RGBA{0xdd, 0xdd, 0xdd, 0xff}, textAlignCenter, 1, fmt.Sprintf("A comic by %v.", script.Author), 3, 0, height-15, width, 20)
}
