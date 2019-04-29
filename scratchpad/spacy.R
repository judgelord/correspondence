
## Part-of-speech tagging

When you first work with text in R, R has no way of knowing if words are nouns, verbs, or adjectives. You can algorithmically predict what part of speech each word is using a part-of-speech tagger, like [`spaCy`](https://spacy.io/) or [Stanford NLP](https://nlp.stanford.edu/). You can do this in R with the [`cleanNLP` package](https://statsmaths.github.io/cleanNLP/), which connects to external natural language processing algorithms like spaCy or Stanford's thing.

Installing `cleanNLP` is trivial—it's just a normal R package, so use the "Packages" panel in RStudio—but connecting it with external NLP algorithms is a little trickier. To install spaCy, which is a really fast tagging library, do this:
  
1. Make sure Python is installed (it is if you're on macOS or Linux; good luck with Windows—I have no idea how to install this stuff there, but there's a way).
2. Open Terminal and run this command to install `spaCy`:
  
```sh
pip install -U spacy
```

3. Run this command to download `spaCy`'s English algorithms:

```sh
python -m spacy download en
```

4. The end!

Here's the general process for tagging (they call it annotating) text:
  
1. Make a dataset where the first column is the id (line number, chapter number, book+chapter, whatever) and the second column is the text itself.
2. Initialize the NLP tagger. You can use an R-only one that doesn't need Python or any other external dependencies with `cnlp_init_udpipe()`. If you've installed spaCy, use `cnlp_init_spacy()`. If you've installed Stanford's thing, use `cnlp_init_corenlp()`.
3. Feed the data frame from step 1 into the `cnlp_annotate()` function and wait.
4. Save the tagged data as a file on your computer so you don't have to retag it every time. Use `cnlp_get_tif() %>% write_csv()`.
5. The end!

```{r pos-tagging, eval=FALSE}
# Wrangle text into format that cnlp_annotate() needs
chapters <- bom %>%
mutate(book_chapter = paste(book_title, chapter_number)) %>%
select(book_title, book_chapter, scripture_text) %>%
nest(scripture_text) %>%
mutate(text = data %>% map_chr(~ paste(.$scripture_text, collapse = " "))) %>%
select(book_chapter, text, book_title)
# Set up NLP backend
# cnlp_init_udpipe() # This NLP engine doesn't need Python, but it's so so so slow
cnlp_init_spacy() # Use spaCy
# Tag all the parts of speech!
bom_annotated <- cnlp_annotate(bom_chapters, as_strings = TRUE)
# Save the tagged data so we don't have to tag it all again
cnlp_get_tif(bom_annotated) %>%
  write_csv(path = "data/bom_annotated.csv")
```
