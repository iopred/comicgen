package comicgen

import (
	"bufio"
	"bytes"
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

	"github.com/golang/freetype/truetype"
	"github.com/llgcode/draw2d"
	"github.com/llgcode/draw2d/draw2dimg"
	"golang.org/x/image/draw"
	"golang.org/x/image/font"
	"golang.org/x/image/math/fixed"
)

const arrowHeight float64 = 5

const (
	textAlignLeft int = iota
	textAlignCenter
	textAlignRight
)

// ComicGen is a comic generator!
type ComicGen struct {
	sync.Mutex
	emoji          map[rune]string
	defaultAvatars []string
	renderers      []cellRenderer
	current        draw.Image
	font           *draw2d.FontData
	glyphBuf       *truetype.GlyphBuf
}

// NewComicGen creates a new comic generator.
func NewComicGen(font string) (*ComicGen, error) {

	var avatarFiles []os.FileInfo
	var err error
	if avatarFiles, err = ioutil.ReadDir("avatars"); err != nil {
		return nil, fmt.Errorf("Could not open avatars directory: %v", err)
	}

	avatars := []string{}
	for _, avatarFile := range avatarFiles {
		if avatarFile.IsDir() {
			continue
		}
		avatars = append(avatars, "avatars/"+avatarFile.Name())
	}

	emoji := map[rune]string{}
	var emojiFiles []os.FileInfo
	if emojiFiles, err = ioutil.ReadDir("emoji"); err != nil {
		return nil, fmt.Errorf("Could not open avatars directory: %v", err)
	}

	for _, emojiFile := range emojiFiles {
		if emojiFile.IsDir() {
			continue
		}
		name := emojiFile.Name()
		if !strings.HasPrefix(name, "emoji_u") {
			continue
		}
		index := strings.Index(name, ".")
		if index == -1 {
			continue
		}
		chars := strings.Split(name[7:index], "_")

		if len(chars) == 1 {
			i, e := strconv.ParseInt(chars[0], 16, 32)
			if e != nil {
				continue
			}

			emoji[rune(i)] = "emoji/" + name
		}
	}

	draw2d.SetFontFolder("fonts")

	return &ComicGen{
		emoji:          emoji,
		defaultAvatars: avatars,
		renderers: []cellRenderer{
			&oneSpeakerCellRenderer{},
			&flippedOneSpeakerCellRenderer{},
			&oneSpeakerMonologueCellRenderer{},
			&twoSpeakerCellRenderer{},
		},
		font:     &draw2d.FontData{font, draw2d.FontFamilySans, draw2d.FontStyleNormal},
		glyphBuf: &truetype.GlyphBuf{},
	}, nil
}

// Message contains a single message for a comment.
type Message struct {
	Speaker int
	Text    string
}

// Script contains all the data necessary to generate a comic.
type Script struct {
	Messages []*Message
	Avatars  map[int]string
	Author   string
}

const maxComicLength = 3

// MaxLines returns the maximum lines a comic can use.
func (comic *ComicGen) MaxLines() int {
	// Determine the longest script possible
	maxLines := 0
	for _, renderer := range comic.renderers {
		if renderer.lines() > maxLines {
			maxLines = renderer.lines()
		}
	}
	return maxLines*maxComicLength - 1
}

// MakeComic makes a comic.
func (comic *ComicGen) MakeComic(script *Script) (image.Image, error) {
	comic.Lock()
	defer comic.Unlock()

	messages := script.Messages

	maxLines := comic.MaxLines()
	if len(messages) > maxLines {
		messages = messages[len(messages)-maxLines:]
	}

	// Create all plans that are sufficient, and pick a random one.
	plans := [][]cellRenderer{}
	planchan := make(chan []cellRenderer, len(comic.renderers)*len(comic.renderers))
	go createPlans(planchan, comic.renderers, maxComicLength, make([]cellRenderer, 0), messages, 0)
	for {
		plan, ok := <-planchan
		if !ok || plan == nil {
			break
		}
		plans = append(plans, plan)
	}

	if len(plans) == 0 {
		return nil, fmt.Errorf("No plans available to render script: %v", messages)
	}
	plan := plans[rand.Intn(len(plans))]

	width := len(plan)*240 - 10

	// Initialize the context.
	comic.current = image.NewRGBA(image.Rect(0, 0, width, 225))
	draw.Draw(comic.current, comic.current.Bounds(), image.White, image.ZP, draw.Src)

	gc := draw2dimg.NewGraphicContext(comic.current)
	gc.SetDPI(72)
	gc.SetFontData(*comic.font)
	gc.SetFont(draw2d.GetFont(*comic.font))

	avatars := map[int]image.Image{}

	for i, url := range script.Avatars {
		if url != "" {
			image, err := fetchAvatar(url)
			if err == nil {
				avatars[i] = image
			}
		}
	}

	seen := map[int]bool{}

	for _, m := range script.Messages {
		if avatars[m.Speaker] == nil {
			for {
				i := rand.Intn(len(comic.defaultAvatars))
				if !seen[i] {
					seen[i] = true
					if file, err := os.Open(comic.defaultAvatars[i]); err == nil {
						defer file.Close()
						if avatar, _, err := image.Decode(bufio.NewReader(file)); err == nil {
							avatars[m.Speaker] = avatar
						}
					}
					break
				}
			}
		}
	}

	for i, c := 0, 0; i < len(plan); i++ {
		renderer := plan[i]
		renderer.render(comic, gc, avatars, messages[c:c+renderer.lines()], 5+240*float64(i), 5, 220, 200)
		c += renderer.lines()
	}
	comic.drawTextInRect(gc, color.RGBA{0xdd, 0xdd, 0xdd, 0xff}, textAlignRight, 1, fmt.Sprintf("A comic by %v.", script.Author), 3, 0, 205, float64(width), 20)

	return comic.current, nil
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

func createPlans(planchan chan []cellRenderer, renderers []cellRenderer, comicLength int, currentPlan []cellRenderer, remainingScript []*Message, currentLength int) {
	if currentLength > comicLength {
		return
	} else if len(remainingScript) == 0 {
		planchan <- currentPlan
		return
	}
	for _, renderer := range renderers {
		if s := renderer.satisfies(remainingScript); s > 0 {
			createPlans(planchan, renderers, comicLength, append(currentPlan, renderer), remainingScript[s:], currentLength+1)
		}
	}
	if currentLength == 0 {
		planchan <- nil
	}
}

func drawSpeech(gc *draw2dimg.GraphicContext, border, radius, x, y, width, height, pointX, pointY float64) {
	gc.Save()
	color := color.Black
	gc.SetLineCap(draw2d.RoundCap)
	gc.SetLineJoin(draw2d.RoundJoin)
	gc.SetLineWidth(border * 2)
	gc.SetStrokeColor(color)
	gc.SetFillColor(color)

	gc.MoveTo(x+radius, y)
	gc.LineTo(x+width-radius, y)
	// top right corner
	gc.QuadCurveTo(x+width, y, x+width, y+radius)
	gc.LineTo(x+width, y+height-radius)
	// botttom right corner
	gc.QuadCurveTo(x+width, y+height, x+width-radius, y+height)
	gc.LineTo(x+radius, y+height)
	// bottom left corner
	gc.QuadCurveTo(x, y+height, x, y+height-radius)
	gc.LineTo(x, y+radius)
	// top left corner
	gc.QuadCurveTo(x, y, x+radius, y)
	// save the bubble area, stroke it, then save it again (so it can be filled with white)
	gc.Save()
	gc.FillStroke()
	gc.Restore()
	gc.Save()

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
	r *= 0.9

	sx := cx + r*nx
	sy := cy + r*ny

	arrowWidth := d * 0.2

	gc.MoveTo(pointX, pointY)
	gc.LineTo(sx+ny*arrowWidth, sy+-nx*arrowWidth)
	gc.LineTo(sx+-ny*arrowWidth, sy+nx*arrowWidth)
	gc.LineTo(pointX, pointY)

	// Save the arrow, then fill it with the outline color
	gc.Save()
	gc.FillStroke()
	gc.Restore()

	// Finally draw the arrow in white, then restore back to our bubble, draw it in white
	gc.SetFillColor(image.White)
	gc.Fill()
	gc.Restore()
	gc.SetFillColor(image.White)
	gc.Fill()

	gc.Restore()
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
				log.Println(err)
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
	gc.Save()
	gc.SetStrokeColor(color)
	gc.SetFillColor(color)

	wrapText, fontSize, _, _ := comic.fitText(gc, spacing, text, width-border*2, height-border*2)

	if fontSize > 40 {
		fontSize = 40
	}

	gc.SetFontSize(fontSize)

	_, t, _, b := comic.textBounds(gc, spacing, wrapText)

	textHeight := -t + b

	top := -t + y + (height-textHeight)/2

	lineHeight := fUnitsToFloat64(gc.Current.Font.VMetric(fixed.Int26_6(gc.Current.Scale), 0).AdvanceHeight) * spacing

	// Draw the text.
	lines := strings.Split(wrapText, "\n")
	for i, line := range lines {
		l, _, r, _ := comic.textBounds(gc, spacing, line)
		textWidth := -l + r
		var px float64
		switch align {
		case textAlignLeft:
			px = x - l + border
		case textAlignCenter:
			px = x - l + (width-textWidth)/2
		case textAlignRight:
			px = width - textWidth - border
		}
		py := top + lineHeight*float64(i)

		comic.createStringPath(gc, line, px, py)
		gc.Fill()
	}
	gc.Restore()
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
	gc.Save()

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

	gc.Restore()
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

func outline(gc *draw2dimg.GraphicContext, x, y, width, height float64) {
	gc.Save()
	color := color.RGBA{0x00, 0x00, 0x00, 0x22}
	gc.SetLineCap(draw2d.RoundCap)
	gc.SetLineJoin(draw2d.RoundJoin)
	gc.SetLineWidth(2)
	gc.SetStrokeColor(color)
	rect(gc, x, y, width, height)
	gc.Stroke()
	gc.Restore()
}

type cellRenderer interface {
	// Returns the maximum number of lines this renderer can satisfy.
	lines() int
	// If this returns a > 0 value, this renderer has said to be able to satisfy that many lines of the script.
	// If this returns 0, this renderer cannot satisfy any lines and is unusable.
	satisfies(messages []*Message) int
	render(comic *ComicGen, gc *draw2dimg.GraphicContext, avatars map[int]image.Image, messages []*Message, x, y, width, height float64)
}

type oneSpeakerCellRenderer struct{}

func (c *oneSpeakerCellRenderer) lines() int {
	return 1
}

func (c *oneSpeakerCellRenderer) satisfies(messages []*Message) int {
	if len(messages) > 0 {
		return 1
	}
	return 0
}

func (c *oneSpeakerCellRenderer) render(comic *ComicGen, gc *draw2dimg.GraphicContext, avatars map[int]image.Image, messages []*Message, x, y, width, height float64) {
	outline(gc, x, y, width, height)

	if len(messages) != c.lines() {
		return
	}

	border := float64(5)
	bounds := image.Rectangle{image.Point{0, 0}, image.Point{88, 88}}

	avatar := avatars[messages[0].Speaker]

	gc.Save()
	gc.ComposeMatrixTransform(draw2d.NewTranslationMatrix(x+border, y+height-border-float64(bounds.Dy())))
	comic.drawImage(gc, avatar, bounds)
	outline(gc, 0, 0, float64(bounds.Dx()), float64(bounds.Dy()))
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
	if len(messages) > 0 && len(messages[0].Text) < 192 {
		return 1
	}
	return 0
}

func (c *flippedOneSpeakerCellRenderer) render(comic *ComicGen, gc *draw2dimg.GraphicContext, avatars map[int]image.Image, messages []*Message, x, y, width, height float64) {
	outline(gc, x, y, width, height)

	if len(messages) != c.lines() {
		return
	}

	border := float64(5)
	bounds := image.Rectangle{image.Point{0, 0}, image.Point{88, 88}}

	avatar := avatars[messages[0].Speaker]

	gc.Save()
	gc.ComposeMatrixTransform(draw2d.NewTranslationMatrix(x+border, y+border))
	comic.drawImage(gc, avatar, bounds)
	outline(gc, 0, 0, float64(bounds.Dx()), float64(bounds.Dy()))
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
	if len(messages) > 1 && countSpeakers(messages[:c.lines()]) == 2 && len(messages[0].Text) < 64 && len(messages[1].Text) < 64 {
		return 2
	}
	return 0
}

func (c *twoSpeakerCellRenderer) render(comic *ComicGen, gc *draw2dimg.GraphicContext, avatars map[int]image.Image, messages []*Message, x, y, width, height float64) {
	outline(gc, x, y, width, height)

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
		outline(gc, 0, 0, float64(bounds.Dx()), float64(bounds.Dy()))
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
	if len(messages) > 1 && countSpeakers(messages[:c.lines()]) == 1 && len(messages[0].Text) < 64 {
		return 2
	}
	return 0
}

func (c *oneSpeakerMonologueCellRenderer) render(comic *ComicGen, gc *draw2dimg.GraphicContext, avatars map[int]image.Image, messages []*Message, x, y, width, height float64) {
	outline(gc, x, y, width, height)

	if len(messages) != c.lines() {
		return
	}

	border := float64(5)
	bounds := image.Rectangle{image.Point{0, 0}, image.Point{88, 88}}

	avatar := avatars[messages[0].Speaker]

	gc.Save()
	gc.ComposeMatrixTransform(draw2d.NewTranslationMatrix(x+border, y+height-border-float64(bounds.Dy())))
	comic.drawImage(gc, avatar, bounds)
	outline(gc, 0, 0, float64(bounds.Dx()), float64(bounds.Dy()))
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
	gc.ComposeMatrixTransform(draw2d.NewScaleMatrix(float64(bounds.Dx())/float64(ib.Dx()), float64(bounds.Dy())/float64(ib.Dy())))
	gc.DrawImage(img)
	gc.Restore()
}
