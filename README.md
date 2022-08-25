# HaskellToybox
A small collection of Haskell toys and tools I've written for fun and for science!

## KHInsiderGetter
### Current version: 0.1.0.0
A bulk downloader for KHInsider albums. Given a link to an album on KHInsider, the program will scrape it for MP3 links and download them in sequence to `./mp3`. This directory has to be created beforehand.

It probably isn't perfect, since it uses shoddy regex to find the links, but it's worked on the few albums I've tried it on.
