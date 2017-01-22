package main

import (
	"bufio"
	"fmt"
	"image/png"
	"math/rand"
	"os"
	"time"

	"github.com/iopred/comicgen"
)

func main() {
	rand.Seed(time.Now().UnixNano())

	c := comicgen.NewComicGen("arial", false)

	i, e := c.MakeComic(&comicgen.Script{
		Messages: []*comicgen.Message{
			{
				Speaker: 0,
				Text:    "Hello World!",
				Author:  "iopred",
			},
			{
				Speaker: 1,
				Text:    "This is some text that will wrap.",
				Author:  "iopred",
			},
			{
				Speaker: 2,
				Text:    "💯 🌝",
				Author:  "iopred",
			},
			{
				Speaker: 3,
				Text:    "SHUT YOUR MOUTH\nBlah\n\nBlah\n",
			},
			{
				Speaker: 4,
				Text:    "This is a story all about how my life got flip-turned upside down and I liked to take a minute and sit right there and tell you how I became the prince of a town called Bel Air. Dooot DoootDooot Dooot DoootDooot DoootDooot Dooot Dooot Dooot Dooot DoootDoootDooot.",
				Author:  "iopredaoeuaeuuaoeuaoeuaoeuaoeuaoeuaoeuaoeuoeuoeuouoeu",
			},
			{
				Speaker: 5,
				Text:    "SHUT YOUR MOUTH!!!",
				Author:  "iopred",
			},
			{
				Speaker: 6,
				Text:    ":(",
				Author:  "iopred aoeuaeu4 234234",
			},
			{
				Speaker: -1,
				Text:    "Hello",
				Author:  "iopred",
			},
			{
				Speaker: -1,
				Text:    "Bye",
				Author:  "iopred",
			},
		},
		Author: "iopred",
		Avatars: map[int]string{
			0: "https://avatars0.githubusercontent.com/u/1529218?v=3&s=460",
		},
		Type: comicgen.ComicTypeChat,
		Room: "#butts",
	})
	if e != nil {
		fmt.Println(e)
		return
	}

	f, _ := os.Create("comic.png")

	w := bufio.NewWriter(f)

	png.Encode(w, i)

	w.Flush()
}
