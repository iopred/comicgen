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
				Text:    "Hello World!",
				Author:  "iopred",
			},
			{
				Text:    "This is some text replacement that will wrap.",
				Author:  "iopred",
				Replacements: map[string]string{
					"replacement": "https://cdn.discordapp.com/emojis/230322831303507968.png",
				},
			},
			{
				Text:    "💯 🌝",
				Author:  "iopred",
			},
			{
				Text:    "SHUT YOUR MOUTH\nBlah\n\nBlah\n",
			},
			{
				Text:    "This replacement ass is a story all about how my life got flip-turned upside down and I liked to take a minute and sit right there and tell you how I became the prince of a town called Bel Air. Dooot DoootDooot Dooot DoootDooot DoootDooot Dooot Dooot Dooot Dooot DoootDoootDooot.",
				Author:  "iopredaoeuaeuuaoeuaoeuaoeuaoeuaoeuaoeuaoeuoeuoeuouoeu",
				Replacements: map[string]string{
					"replacement": "https://cdn.discordapp.com/emojis/230322831303507968.png",
					"ass":         "https://cdn.discordapp.com/emojis/243513506358362112.png",
				},
			},
			{
				Text:    "SHUT YOUR MOUTH!!!",
				Author:  "iopred",
			},
			{
				Text:    ":(",
				Author:  "iopred aoeuaeu4 234234",
			},
			{
				Text:    "Hello",
				Author:  "iopred",
			},
			{
				Text:    "Bye",
				Author:  "iopred",
			},
		},
		Author: "iopred",
		Avatars: map[string]string{
			"iopred": "https://avatars0.githubusercontent.com/u/1529218?v=3&s=460",
		},
		Type: comicgen.ComicTypeTragedy,
		Room: "#butts",
	})
	if e != nil {
		fmt.Printf("Error %s\n", e.Error())
		return
	}

	f, _ := os.Create("comic.png")

	w := bufio.NewWriter(f)

	png.Encode(w, i)

	w.Flush()
}
