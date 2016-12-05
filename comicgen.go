package comicgen

import (
	"bufio"
	"bytes"
	"encoding/json"
	"errors"
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

// MultiBounds is a struct containing bounds information for multiple lines of text.
type MultiBounds struct {
	AllBounds  [][]float64
	LineHeight float64
}

// Length returns the number of lines contained in the MultiBounds.
func (m *MultiBounds) Length() int {
	return len(m.AllBounds)
}

// Bounds returns the total bounds of all lines.
func (m *MultiBounds) Bounds() (l, t, r, b float64) {
	for _, bound := range m.AllBounds {
		if bound[0] < l {
			l = bound[0]
		}
		if bound[1] < t {
			t = bound[1]
		}
		if bound[2] > r {
			r = bound[2]
		}
		if bound[3] > b {
			b = bound[3]
		}
	}
	return
}

// AddBound will add a new bound.
func (m *MultiBounds) AddBound(l, t, r, b float64) {
	m.AllBounds = append(m.AllBounds, []float64{l, t, r, b})
}

// AddStringBounds will add a new bound and automodify the bound to be positioned on the next line.
func (m *MultiBounds) AddStringBounds(l, t, r, b float64) {
	y := float64(m.Length()) * m.LineHeight
	m.AddBound(l, t+y, r, b+y)
}

// GetBound returns the bound at the desired line.
func (m *MultiBounds) GetBound(i int) (float64, float64, float64, float64) {
	b := m.AllBounds[i]
	return b[0], b[1], b[2], b[3]
}

// Offset returns a new MultiBound offset by another.
func (m *MultiBounds) Offset(offset *MultiBounds) *MultiBounds {
	c := &MultiBounds{}

	for i, b := range m.AllBounds {
		c.AddBound(b[0]+offset.AllBounds[i][0], b[1]+offset.AllBounds[i][1], b[2]+offset.AllBounds[i][2], b[3]+offset.AllBounds[i][3])
	}

	return c
}

const arrowHeight float64 = 5

const (
	textAlignLeft int = iota
	textAlignCenter
	textAlignRight
)

// An Emotion is a possible state for a character in a comic.
type Emotion string

// All the possible emotion states.
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
	EmotionAngry:    {"grr", "fuck", "damned", "goddamned", "shit", "angry", "upset", "crap", "😬", "😐", "😠", "😤", ">_<"},
	EmotionBored:    {"sigh", "boring", "bored", "...", "💤", "😴", ":|"},
	EmotionHappy:    {"^-^", "^_^", ":)", "yay", "happy", "awesome", "awesome", "great", "good", "😀", "😬", "😁", "😂", "😃", "😄", "😅", "😆", "😉", "😊", "🙂", "😋", "😌", "😜", "😝", "😛", "😏"},
	EmotionHi:       {"Hello", "Hi", "Yo", "Hey", "Heya"},
	EmotionLove:     {":P", "<3", "❤️", "💛", "💙", "💜", "💔", "❣️", "💕", "💞", "💓", "💗", "💖", "💘", "💝", "💟"},
	EmotionMe:       {"me", "myself", "i'm", "i am", "im "},
	EmotionYou:      {"you", "you're", "you are", "youre"},
	EmotionQuestion: {"?", "what", "wat", "huh"},
	EmotionSad:      {"-_-", ":(", "sad", "bad", "😧", "😢", "😓", "😭", "😰"},
	EmotionScared:   {"no", "ack", "arg", "scared", "uh oh", "😱", "😳"},
	EmotionSmug:     {"of course", "obviously", "i know", "duh", "pff"},
	EmotionYell:     {"!"},
	EmotionWalk:     {"bye", "later"},
}

// A Character contains the information necessary to draw one character in a comic.
type Character struct {
	FileName string
	Name     string
	Width    int
	Height   int
	Frames   int
	Emotions map[Emotion][]int
}

// GetFrame will return a frame for a message. It will parse any emotion from the message and randomly select a matching frame for those emotions.
func (c *Character) GetFrame(message string) int {
	possible := []Emotion{}
	for e, p := range emotionTriggers {
		for _, s := range p {
			if strings.Index(message, s) != -1 {
				possible = append(possible, e)
			}
		}
	}

	if strings.ToUpper(message) == message {
		possible = append(possible, EmotionYell)
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

// MaxFrame returns the highest frame number available to the character.
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
var defaultCharactersMap = map[string]int{}
var defaultRooms = []string{}
var CharacterNames = []string{}

// A ComicType specifies the type of comic to create.
type ComicType int

const (
	// ComicTypeSimple generates a comic with large text bubbles and avatars.
	ComicTypeSimple ComicType = iota
	// ComicTypeChat emulates Microsoft Comic Chat and creates a comic with characters and backgrounds.
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
		log.Printf("Could not open emoji directory: %s %v\n", path, err)
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
		log.Printf("Could not open avatars directory: %v\n", err)
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
		log.Printf("Could not open rooms directory: %v\n", err)
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
		log.Printf("Could not open characters directory: %v\n", err)
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
			defaultCharactersMap[n] = len(defaultCharacters) - 1
			CharacterNames = append(CharacterNames, n)
		}
	}

	draw2d.SetFontFolder("fonts")
}

func loadEmotions(path string) (e map[Emotion][]int) {
	e = map[Emotion][]int{}

	file, err := os.Open(path)
	if err != nil {
		return
	}

	d := json.NewDecoder(bufio.NewReader(file))
	d.Decode(&e)

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

type finishedFunc func([]cellRenderer) bool

func finishedFuncForType(ct ComicType) finishedFunc {
	switch ct {
	case ComicTypeChat:
		return finishedFunc(func(currentPlan []cellRenderer) bool {
			return (len(currentPlan) < 4 || len(currentPlan)%3 == 2)
		})
	}
	return finishedFunc(func(currentPlan []cellRenderer) bool {
		return len(currentPlan) < 4 || len(currentPlan)%3 == 0
	})
}

// MakeComic makes a comic.
func (comic *ComicGen) MakeComic(script *Script) (img image.Image, err error) {
	defer func() {
		if r := recover(); r != nil {
			err = errors.New("")
		}
	}()

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

	seen := map[int]bool{}

	if script.Type != ComicTypeChat {
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
	} else {
		comic.room, _ = loadImage(defaultRooms[rand.Intn(len(defaultRooms))])

		for _, m := range script.Messages {
			if m.Speaker == -1 {
				if i, ok := defaultCharactersMap[m.Author]; ok {
					m.Speaker = i
					if !seen[i] {
						seen[i] = true
						comic.characters[m.Speaker] = defaultCharacters[i]
						if img, err := loadImage(defaultCharacters[i].FileName); err == nil {
							comic.characterImages[m.Speaker] = img
						}
					}
				} else {
					m.Speaker = 0
				}
				m.Author = ""
			}
		}

		tried := 0
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
					tried++
					if tried > 1000 {
						return nil, errors.New("Too many characters.")
					}
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
		comic.drawTextInRect(color.RGBA{0xdd, 0xdd, 0xdd, 0xff}, textAlignRight, 1, fmt.Sprintf("A comic by %s.", script.Author), 3, 0, float64(height)-20, float64(width), 20)
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

func (comic *ComicGen) drawSpeech(border, radius, x, y, width, height, pointX, pointY float64) {
	gc := comic.gc
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

func (comic *ComicGen) loadGlyph(glyph truetype.Index) error {
	gc := comic.gc
	return comic.glyphBuf.Load(gc.Current.Font, fixed.Int26_6(gc.Current.Scale), glyph, font.HintingNone)
}

func (comic *ComicGen) drawGlyph(glyph truetype.Index, dx, dy float64) error {
	gc := comic.gc
	if err := comic.loadGlyph(glyph); err != nil {
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
	file, err := os.Open(comic.emoji[r])
	if err != nil {
		return err
	}
	defer file.Close()

	emoji, _, err := image.Decode(bufio.NewReader(file))
	if err != nil {
		return err
	}

	if width > height {
		height = width
	}

	gc.Save()
	gc.ComposeMatrixTransform(draw2d.NewTranslationMatrix(x, y))
	comic.drawImage(emoji, image.Rectangle{image.Point{0, 0}, image.Point{int(width), int(height)}})
	gc.Restore()

	return nil
}

func (comic *ComicGen) createStringPath(s string, x, y float64) {
	gc := comic.gc
	font := gc.Current.Font
	prev, hasPrev := truetype.Index(0), false
	for _, r := range s {
		index := font.Index(r)
		if hasPrev {
			x += fUnitsToFloat64(font.Kern(fixed.Int26_6(gc.Current.Scale), prev, index))
		}

		if comic.emoji[r] != "" {
			l, t, ri, b := comic.getStringBounds(string(r))
			comic.drawEmoji(gc, r, x+l, y+t, ri-l, b-t)
		} else {
			err := comic.drawGlyph(index, x, y)
			if err != nil {
				return
			}
		}
		x += fUnitsToFloat64(font.HMetric(fixed.Int26_6(gc.Current.Scale), index).AdvanceWidth)
		prev, hasPrev = index, true
	}
}

func (comic *ComicGen) getStringBounds(s string) (left, top, right, bottom float64) {
	gc := comic.gc
	font := gc.Current.Font
	top, left, bottom, right = 0, 0, 0, 0
	cursor := 0.0
	prev, hasPrev := truetype.Index(0), false
	for _, rune := range s {
		index := font.Index(rune)
		if hasPrev {
			cursor += fUnitsToFloat64(font.Kern(fixed.Int26_6(gc.Current.Scale), prev, index))
		}

		if err := comic.loadGlyph(index); err != nil {
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
	return
}

func (comic *ComicGen) drawTextInRect(color color.Color, align int, spacing float64, text string, border, x, y, width, height float64) {
	wrapText, fontSize, _, _ := comic.fitText(spacing, text, width-border*2, height-border*2)

	if fontSize > 40 {
		fontSize = 40
	}

	comic.drawText(color, fontSize, align, spacing, wrapText, x+border, y, width-border*2, height)
}

func (comic *ComicGen) alignMultiBounds(mb *MultiBounds, align int, width, height float64) *MultiBounds {

	nmb := &MultiBounds{
		LineHeight: mb.LineHeight,
	}

	_, t, _, b := mb.Bounds()
	textHeight := b - t

	y := 0.0
	if height != 0 {
		y += (height - textHeight) / 2
	}

	for i := 0; i < mb.Length(); i++ {
		l, _, r, _ := mb.GetBound(i)

		textWidth := r - l
		var x float64
		switch align {
		case textAlignCenter:
			x = (width - textWidth) / 2
		case textAlignRight:
			x = width - textWidth
		}

		nmb.AddBound(x, y, x, y)
	}

	return nmb
}

func (comic *ComicGen) drawText(color color.Color, fontSize float64, align int, spacing float64, text string, x, y, width, height float64) *MultiBounds {
	gc := comic.gc

	gc.Save()
	defer gc.Restore()

	gc.SetStrokeColor(color)
	gc.SetFillColor(color)
	gc.SetFontSize(fontSize)

	mb := comic.textBounds(spacing, text)

	ox, oy, _, _ := mb.Bounds()

	amb := comic.alignMultiBounds(mb, align, width, height)

	gc.ComposeMatrixTransform(draw2d.NewTranslationMatrix(x-ox, y-oy))

	// Draw the text.
	lines := strings.Split(text, "\n")
	for i, line := range lines {
		l, t, _, _ := amb.GetBound(i)
		comic.createStringPath(line, l, t+float64(i)*mb.LineHeight)
		gc.Fill()
	}

	return mb
}

func fUnitsToFloat64(x fixed.Int26_6) float64 {
	scaled := x << 2
	return float64(scaled/256) + float64(scaled%256)/256.0
}

func pointToF64Point(p truetype.Point) (x, y float64) {
	return fUnitsToFloat64(p.X), -fUnitsToFloat64(p.Y)
}

func (comic *ComicGen) textBounds(spacing float64, text string) *MultiBounds {
	gc := comic.gc
	mb := &MultiBounds{
		LineHeight: fUnitsToFloat64(gc.Current.Font.VMetric(fixed.Int26_6(gc.Current.Scale), 0).AdvanceHeight) * spacing,
	}

	lines := strings.Split(text, "\n")
	for _, line := range lines {
		mb.AddStringBounds(comic.getStringBounds(line))
	}
	return mb
}

func (comic *ComicGen) textSize(spacing float64, text string) (width, height float64) {
	l, t, r, b := comic.textBounds(spacing, text).Bounds()
	return r - l, b - t
}

func (comic *ComicGen) fitText(spacing float64, text string, width, height float64) (wrappedText string, fontSize, wrapWidth, wrapHeight float64) {
	if text == "" {
		return text, 1, 0, 0
	}
	gc := comic.gc
	gc.Save()
	defer gc.Restore()

	// Match aspect ratios, favoring width.
	aspect := width / height
	for low, high := 1.0, 100.0; high > low+0.1; {
		fontSize = low + (high-low)/2

		gc.SetFontSize(fontSize)

		wrappedText = comic.wrapText(spacing, text, width)
		wrapWidth, wrapHeight = comic.textSize(spacing, wrappedText)
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

func (comic *ComicGen) fitTextHeight(fs float64, spacing float64, text string, width, height float64) (wrappedText string, fontSize float64, mb *MultiBounds) {
	gc := comic.gc
	gc.Save()
	defer gc.Restore()

	fontSize = fs

	for {
		gc.SetFontSize(fontSize)

		wrappedText = comic.wrapText(spacing, text, width)

		mb = comic.textBounds(spacing, wrappedText)

		l, t, r, b := mb.Bounds()

		if b-t < height && r-l < width {
			break
		}
		if fontSize == 1 {
			break
		}
		fontSize--
	}
	return
}

func (comic *ComicGen) wrapText(spacing float64, text string, wrapWidth float64) string {
	var buffer bytes.Buffer
	var maxWidth float64
	lines := strings.Split(text, "\n")
	for i, line := range lines {
		width := comic.wrapLine(&buffer, spacing, line, wrapWidth)
		if width > maxWidth {
			maxWidth = width
		}
		if i < len(lines)-1 {
			buffer.WriteString("\n")
		}
	}
	return buffer.String()
}

func (comic *ComicGen) wrapLine(buffer *bytes.Buffer, spacing float64, line string, wrapWidth float64) float64 {
	var width float64
	var runningWidth float64
	var maxWidth float64
	words := strings.Split(line, " ")
	for i, word := range words {
		if i != 0 {
			width, _ = comic.textSize(spacing, " "+word)
		} else {
			width, _ = comic.textSize(spacing, word)
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

var outlineColor = color.RGBA{0x00, 0x00, 0x00, 0x22}

func outline(gc *draw2dimg.GraphicContext, color color.Color, x, y, width, height, strokewidth float64) {
	gc.Save()
	defer gc.Restore()

	gc.SetLineCap(draw2d.ButtCap)
	gc.SetLineJoin(draw2d.MiterJoin)
	gc.SetLineWidth(strokewidth)
	gc.SetStrokeColor(color)
	draw2dkit.Rectangle(gc, x, y, x+width, x+height)
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
	comic.drawImage(avatar, bounds)
	outline(gc, outlineColor, 0, 0, float64(bounds.Dx()), float64(bounds.Dy()), 2)
	gc.Restore()

	bX, bY, bWidth, bHeight := insetRectangleLRTB(x, y, width, height, border, border, border, border+float64(bounds.Dy())+arrowHeight*2)

	comic.drawSpeech(2, border, bX, bY, bWidth, bHeight, bX+rand.Float64()*float64(bounds.Dx()), bY+bHeight+arrowHeight)
	comic.drawTextInRect(image.Black, textAlignCenter, 1, string(messages[0].Text), arrowHeight, bX, bY, bWidth, bHeight)
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
	comic.drawImage(avatar, bounds)
	outline(gc, outlineColor, 0, 0, float64(bounds.Dx()), float64(bounds.Dy()), 2)
	gc.Restore()

	bX, bY, bWidth, bHeight := insetRectangleLRTB(x, y, width, height, border, border, border+float64(bounds.Dy())+arrowHeight*2, border)

	comic.drawSpeech(2, border, bX, bY, bWidth, bHeight, bX+rand.Float64()*float64(bounds.Dx()), bY-arrowHeight)
	comic.drawTextInRect(image.Black, textAlignCenter, 1, string(messages[0].Text), arrowHeight, bX, bY, bWidth, bHeight)
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
		comic.drawImage(avatar, bounds)
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

		comic.drawSpeech(2, border, bX, bY, bWidth, bHeight, bX+arrowX, bY+rand.Float64()*float64(bounds.Dx()))
		comic.drawTextInRect(image.Black, textAlignCenter, 1, string(messages[i].Text), 10, bX, bY, bWidth, bHeight)

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
	comic.drawImage(avatar, bounds)
	outline(gc, outlineColor, 0, 0, float64(bounds.Dx()), float64(bounds.Dy()), 2)
	gc.Restore()

	bX, bY, bWidth, bHeight := insetRectangleLRTB(x, y, width, height, border, border, border, border+float64(bounds.Dy())+arrowHeight*2)

	comic.drawSpeech(2, border, bX, bY, bWidth, bHeight, bX+rand.Float64()*float64(bounds.Dx()), bY+bHeight+arrowHeight)
	comic.drawTextInRect(image.Black, textAlignCenter, 1, string(messages[0].Text), arrowHeight, bX, bY, bWidth, bHeight)

	bX, bY, bWidth, bHeight = insetRectangleLRTB(x, y, width, height, border+float64(bounds.Dx())+arrowHeight*3, border, y+height-border*2-float64(bounds.Dy()), border)

	comic.drawSpeech(2, border, bX, bY, bWidth, bHeight, bX-arrowHeight*2, bY+rand.Float64()*float64(bounds.Dy()))
	comic.drawTextInRect(image.Black, textAlignCenter, 1, string(messages[1].Text), arrowHeight, bX, bY, bWidth, bHeight)
}

func (comic *ComicGen) drawImage(img image.Image, bounds image.Rectangle) {
	if img == nil {
		return
	}
	gc := comic.gc
	gc.Save()
	defer gc.Restore()

	ib := img.Bounds()
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

func (comic *ComicGen) drawCharacter(sub *image.RGBA, message *Message, zoom float64, width, height, position float64, flip float64) {
	characterimg := comic.characterImages[message.Speaker]
	character := comic.characters[message.Speaker]

	frame := character.GetFrame(message.Text)

	b := characterimg.Bounds()

	ox := (character.Width * frame) % b.Dx()
	oy := ((character.Width * frame) / b.Dx()) * character.Height

	tr := draw2d.NewIdentityMatrix()
	tr.Compose(comic.gc.Current.Tr)

	mx := width - float64(character.Width)*zoom*0.5

	tr.Compose(draw2d.NewTranslationMatrix(width*position+mx*0.5, height*0.5-10.0))
	tr.Compose(draw2d.NewScaleMatrix(flip*zoom*0.5, zoom*0.5))
	tr.Compose(draw2d.NewTranslationMatrix(float64(-ox), float64(-oy)))
	if flip == -1 {
		tr.Compose(draw2d.NewTranslationMatrix(-float64(character.Width), 0))
	}

	draw.CatmullRom.Transform(sub, f64.Aff3{tr[0], tr[1], tr[4], tr[2], tr[3], tr[5]}, characterimg, image.Rect(ox, oy, ox+character.Width, oy+character.Height), draw.Over, nil)
}

func (comic *ComicGen) drawComicBubble(mb *MultiBounds, amb *MultiBounds, arrowx, arrowy float64) {
	le := mb.Length()
	if le == 0 {
		return
	}

	gc := comic.gc
	gc.Save()
	defer gc.Restore()

	ox, oy, _, _ := mb.Bounds()
	gc.ComposeMatrixTransform(draw2d.NewTranslationMatrix(-ox, -oy))

	mb = mb.Offset(amb)

	shapes := []*draw2d.Path{}

	size := 5.0
	sizeh := 3.0

	for i := 0; i < mb.Length(); i++ {
		l, t, r, b := mb.GetBound(i)
		w := r - l

		s := &draw2d.Path{}
		draw2dkit.RoundedRectangle(s, l-size, t-sizeh, r+size, b+sizeh, size*4, sizeh*4)
		shapes = append(shapes, s)

		divis := 40.0
		if i == 0 {
			divis *= 0.5
		}
		for j := 0; j < int(math.Min(100, w)/divis); j++ {
			bw := w*0.4 + rand.Float64()*w*0.6
			s := &draw2d.Path{}
			draw2dkit.Ellipse(s, l+bw*0.5+rand.Float64()*(w-bw), t-sizeh*0.5+rand.Float64()*sizeh*0.5, bw*0.5, sizeh*0.5+rand.Float64()*sizeh*0.5)
			shapes = append(shapes, s)
		}

		divis = 40.0
		if i == 0 {
			divis *= 0.5
		}
		for j := 0; j < int(math.Min(100, w)/divis); j++ {
			bw := w*0.4 + rand.Float64()*w*0.6
			s := &draw2d.Path{}
			draw2dkit.Ellipse(s, l+bw*0.5+rand.Float64()*(w-bw), b+sizeh*0.5+rand.Float64()*sizeh*0.5, bw*0.5, sizeh*0.5+rand.Float64()*sizeh*0.5)
			shapes = append(shapes, s)
		}
	}

	l, _, r, b := mb.GetBound(mb.Length() - 1)
	w := r - l
	s := &draw2d.Path{}

	al := l + 2.5 + rand.Float64()*(w-5)
	ar := al + 5

	if w < 5 {
		al = l
		ar = r
	}

	s.MoveTo(al, b-3)
	s.LineTo(ar, b-3)
	s.QuadCurveTo(ar, b-3+(arrowy-b-3)*0.75, arrowx, arrowy)
	s.QuadCurveTo(al, b-3+(arrowy-b-3)*0.75, al, b-3)

	shapes = append(shapes, s)

	gc.SetLineCap(draw2d.RoundCap)
	gc.SetLineJoin(draw2d.RoundJoin)
	gc.SetLineWidth(2)
	gc.SetStrokeColor(color.Black)
	gc.SetFillColor(color.White)

	for _, s := range shapes {
		gc.FillStroke(s)
	}
	for _, s := range shapes {
		gc.Fill(s)
	}
}

func (comic *ComicGen) drawComicSpeech(message *Message, x, y, width, height, arrowx float64) float64 {
	gc := comic.gc
	gc.Save()
	defer gc.Restore()

	fontSize := 14.0
	gc.SetFontSize(fontSize)

	text := comic.wrapText(1, message.Text, width-chatBorder*2)

	mb := comic.textBounds(1, text)

	l, t, r, b := mb.Bounds()

	w := r - l
	h := b - t

	if y+h > height-chatBorder*2 || w > width-chatBorder*2 {
		text, fontSize, mb = comic.fitTextHeight(14, 1, message.Text, width-chatBorder*2, height-chatBorder*2)
		l, t, r, b = mb.Bounds()
		w = r - l
		h = b - t
	}

	ox := math.Min(math.Max(chatBorder, width-(w+chatBorder*2)), math.Max(chatBorder, arrowx-w+w*rand.Float64()))

	gc.ComposeMatrixTransform(draw2d.NewTranslationMatrix(x+ox, y))

	comic.drawComicBubble(mb, comic.alignMultiBounds(mb, textAlignCenter, w, 0), arrowx-(x+ox), height-y)
	comic.drawText(color.Black, fontSize, textAlignCenter, 1, text, 0, 0, w, 0)

	return y + h + chatBorder
}

func (comic *ComicGen) drawComic(messages []*Message, x, y, width, height float64) {
	gc := comic.gc
	gc.Save()
	defer gc.Restore()

	tr := draw2d.NewTranslationMatrix(x, y)
	gc.ComposeMatrixTransform(tr)

	if len(messages) < 1 || len(messages) > 2 {
		return
	}

	zoom := 1.0 + rand.Float64()

	// Draw background
	rb := comic.room.Bounds()
	tr.Compose(draw2d.NewScaleMatrix(width/float64(rb.Dx()), height/float64(rb.Dy())))
	tr.Compose(draw2d.NewTranslationMatrix(0, -(height*zoom-height)*0.5))
	tr.Compose(draw2d.NewScaleMatrix(zoom, zoom))

	sub := comic.img.SubImage(image.Rect(int(x), int(y), int(x+width), int(y+height))).(*image.RGBA)
	draw.BiLinear.Transform(sub, f64.Aff3{tr[0], tr[1], tr[4], tr[2], tr[3], tr[5]}, comic.room, rb, draw.Over, nil)

	top := height*0.5 - 18

	if len(messages) == 1 {
		comic.drawComicSpeech(messages[0], chatBorder, chatBorder, width-chatBorder*2, top, width*0.5)
		comic.drawCharacter(sub, messages[0], zoom, width, height, 0, 1)
	} else {
		bottom := comic.drawComicSpeech(messages[0], chatBorder, chatBorder, width-chatBorder*2, top, width*0.25)

		comic.drawComicSpeech(messages[1], chatBorder, bottom, width-chatBorder*2, top, width*0.75)
		comic.drawCharacter(sub, messages[0], zoom, width*0.5, height, 0, 1)
		comic.drawCharacter(sub, messages[1], zoom, width*0.5, height, 1, -1)
	}

	outline(gc, color.Black, 2, 2, width-4, height-4, 4)
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

func (comic *ComicGen) drawIntroCharacter(id int, name string, width, y float64) {
	gc := comic.gc
	gc.Save()
	defer gc.Restore()

	if name == "" {
		name = comic.characters[id].Name
	}

	characterimg := comic.characterImages[id]

	wrapText, fontSize, mb := comic.fitTextHeight(16, 1, name, width-76, 15)

	l, t, r, b := mb.Bounds()
	w := r - l
	h := b - t

	hasAvatar := comic.avatars[id] != nil

	totalwidth := w + 32
	if hasAvatar {
		totalwidth += 28
	}

	ox := (width - totalwidth) / 2

	gc.ComposeMatrixTransform(draw2d.NewTranslationMatrix(ox, y))

	gc.Save()
	gc.ComposeMatrixTransform(draw2d.NewScaleMatrix(0.75, 0.75))
	tr := gc.Current.Tr
	draw.CatmullRom.Transform(comic.img, f64.Aff3{tr[0], tr[1], tr[4], tr[2], tr[3], tr[5]}, characterimg, image.Rect(3, 3, 40, 40), draw.Over, nil)
	gc.Restore()

	gc.ComposeMatrixTransform(draw2d.NewTranslationMatrix(32, 0))

	if hasAvatar {
		gc.Save()
		gc.ComposeMatrixTransform(draw2d.NewTranslationMatrix(0, 3))
		comic.drawImage(comic.avatars[id], image.Rectangle{image.Point{0, 0}, image.Point{26, 26}})

		gc.SetFillColor(color.White)
		draw2dkit.Rectangle(gc, -1, -1, 28, 28)
		draw2dkit.Circle(gc, 13, 13, 13)
		gc.Fill()

		gc.Restore()
		gc.ComposeMatrixTransform(draw2d.NewTranslationMatrix(28, 0))
	}

	comic.drawText(color.Black, fontSize, textAlignLeft, 1, wrapText, 0, (35-h)*0.5, w, h)
}

func (comic *ComicGen) drawIntro(script *Script, x, y, width, height float64) {
	gc := comic.gc
	gc.Save()
	defer gc.Restore()

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

	gc.Save()
	gc.ComposeMatrixTransform(draw2d.NewTranslationMatrix(x, y+oy))
	comic.drawText(color.Black, 16, textAlignCenter, 1, "A comic starring:", 0, 0, width, 20)

	iy := 20.0

	seen = map[int]bool{}
	for _, m := range script.Messages {
		if !seen[m.Speaker] {
			comic.drawIntroCharacter(m.Speaker, m.Author, width, iy)
			iy += 30
			seen[m.Speaker] = true
		}
		if len(seen) > 5 {
			break
		}
	}
	gc.Restore()
	comic.drawTextInRect(color.RGBA{0xdd, 0xdd, 0xdd, 0xff}, textAlignCenter, 1, fmt.Sprintf("A comic by %s.", script.Author), 3, 0, height-15, width, 20)
}
