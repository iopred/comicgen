package main

import (
	"bufio"
	"image/png"
	"math/rand"
	"os"
	"time"

	"github.com/iopred/comicgen"
)

func main() {
	rand.Seed(time.Now().UnixNano())

	c, _ := comicgen.NewComicGen()

	i, _ := c.MakeComic(&comicgen.Script{
		Messages: []*comicgen.Message{
			{
				Speaker: 0,
				Text:    "Hello World!",
			},
			{
				Speaker: 1,
				Text:    "Foo!",
			},
			{
				Speaker: 1,
				Text:    "Bar!",
			},
			{
				Speaker: 0,
				Text:    "This is a story all about how my life got flip-turned upside down and I liked to take a minute and sit right there and tell you how I became the prince of a town called Bel Air",
			},
		},
		Author: "iopred",
		Avatars: map[int]string{
			0: "https://avatars0.githubusercontent.com/u/1529218?v=3&s=460",
			1: "https://avatars0.githubusercontent.com/u/1523217?v=3&s=460",
		},
	})

	f, _ := os.Create("comic.png")

	w := bufio.NewWriter(f)

	png.Encode(w, i)

	w.Flush()
}
