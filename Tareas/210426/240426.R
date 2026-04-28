install.packages("janeaustenr")
library(janeaustenr)
library(dplyr)
library(stringr)

#chapter:detecta de la columna text exprecciones reg.
#mutate: crea columnas 
original_books <- austen_books() |>
  group_by(book) |>
  mutate(
    line = row_number(),
    chapter = cumsum(str_detect(
      text,
      regex("^chapter [\\divxlc]", ignore_case = TRUE)
    ))
  ) |>
  ungroup()

original_books

install.packages("tidytext")
library("tidytext")

library(tidytext)
tidy_books <- original_books |>
  unnest_tokens(output = word, input = text)

tidy_books


cleaned_books <- tidy_books |>
  anti_join(get_stopwords())

cleaned_books |>
  count(word, sort = TRUE)



positive <- get_sentiments("bing") |>
  filter(sentiment == "positive")

tidy_books |>
  filter(book == "Emma") |>
  semi_join(positive) |>
  count(word, sort = TRUE)




library(tidyr)
bing <- get_sentiments("bing")

janeaustensentiment <- tidy_books |>
  inner_join(bing, relationship = "many-to-many") |>
  count(book, index = line %/% 80, sentiment) |>
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
  mutate(sentiment = positive - negative)

library(ggplot2)

ggplot(janeaustensentiment, aes(index, sentiment, fill = book)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(vars(book), ncol = 2, scales = "free_x")



library(ggplot2)

ggplot(janeaustensentiment, aes(index, sentiment, fill = book)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(vars(book), ncol = 2, scales = "free_x")


bing_word_counts |>
  group_by(sentiment) |>
  slice_max(n, n = 10) |>
  mutate(word = reorder(word, n)) |>
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(sentiment), scales = "free_y") +
  labs(x = "Contribution to sentiment", y = NULL)


bing_word_counts <- tidy_books %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE)



install.packages("wordcloud")
library(reshape2)
library(reshape2)
tidy_books |>
  inner_join(bing) |>
  count(word, sentiment, sort = TRUE) |>
  acast(word ~ sentiment, value.var = "n", fill = 0) |>
  comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words = 100)
library(wordcloud)

cleaned_books |>
  count(word) |>
  with(wordcloud(word, n, max.words = 100))
library(reshape2)

tidy_books |>
  inner_join(bing) |>
  count(word, sentiment, sort = TRUE) |>
  acast(word ~ sentiment, value.var = "n", fill = 0) |>
  comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words = 100)



install.packages("gutenbergr")
library("gutenbergr")

library(gutenbergr)

hgwells <- gutenberg_download(c(35, 36, 5230, 159))

tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_bronte %>%
  count(word, sort = TRUE)
#> # A tibble: 23,042 × 2
#>    word       n
#>    <chr>  <int>
#>  1 time    1065
#>  2 miss     855
#>  3 day      827
#>  4 hand     768
#>  5 eyes     713
#>  6 night    647
#>  7 heart    638
#>  8 looked   602
#>  9 door     592
#> 10 half     586
#> # ℹ 23,032 more rows
#> 
#> 
#> 
library(tidyverse)



# O si prefieres solo la específica:
library(dplyr)

library(tidyr)

frequency <- bind_rows(mutate(tidy_bronte, author = "Brontë Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"), 
                       mutate(tidy_books, author = "Jane Austen")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = author, values_from = proportion) %>%
  pivot_longer(`Brontë Sisters`:`H.G. Wells`,
               names_to = "author", values_to = "proportion")

frequency
#> # A tibble: 57,812 × 4
#>    word    `Jane Austen` author          proportion
#>    <chr>           <dbl> <chr>                <dbl>
#>  1 a          0.00000919 Brontë Sisters  0.00000797
#>  2 a          0.00000919 H.G. Wells     NA         
#>  3 a'most    NA          Brontë Sisters  0.0000159 
#>  4 a'most    NA          H.G. Wells     NA         
#>  5 aback     NA          Brontë Sisters  0.00000398
#>  6 aback     NA          H.G. Wells      0.0000150 
#>  7 abaht     NA          Brontë Sisters  0.00000398
#>  8 abaht     NA          H.G. Wells     NA         
#>  9 abandon   NA          Brontë Sisters  0.0000319 
#> 10 abandon   NA          H.G. Wells      0.0000150 
#> # ℹ 57,802 more rows
#> 
#> 
#>

library(tidyverse)
library(tidytext)

options(gutenbergr_mirror = "http://aleph.gutenberg.org")

library(scales)

# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `Jane Austen`, 
                      color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)


library(tidyverse)
library(tidytext)
library(gutenbergr)
library(scales)

options(gutenbergr_mirror = "http://aleph.gutenberg.org")




#cap3


library(dplyr)
library(janeaustenr)
library(tidytext)

book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE)

total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

book_words
#> # A tibble: 40,378 × 4
#>    book              word      n  total
#>    <fct>             <chr> <int>  <int>
#>  1 Mansfield Park    the    6206 160465
#>  2 Mansfield Park    to     5475 160465
#>  3 Mansfield Park    and    5438 160465
#>  4 Emma              to     5239 160996
#>  5 Emma              the    5201 160996
#>  6 Emma              and    4896 160996
#>  7 Mansfield Park    of     4778 160465
#>  8 Pride & Prejudice the    4331 122204
#>  9 Emma              of     4291 160996
#> 10 Pride & Prejudice to     4162 122204
#> # ℹ 40,368 more rows


library(ggplot2)

ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")

dev.off()



freq_by_rank <- book_words %>% 
  group_by(book) %>% 
  mutate(rank = row_number(), 
         term_frequency = n/total) %>%
  ungroup()

freq_by_rank
#> # A tibble: 40,378 × 6
#>    book              word      n  total  rank term_frequency
#>    <fct>             <chr> <int>  <int> <int>          <dbl>
#>  1 Mansfield Park    the    6206 160465     1         0.0387
#>  2 Mansfield Park    to     5475 160465     2         0.0341
#>  3 Mansfield Park    and    5438 160465     3         0.0339
#>  4 Emma              to     5239 160996     1         0.0325
#>  5 Emma              the    5201 160996     2         0.0323
#>  6 Emma              and    4896 160996     3         0.0304
#>  7 Mansfield Park    of     4778 160465     4         0.0298
#>  8 Pride & Prejudice the    4331 122204     1         0.0354
#>  9 Emma              of     4291 160996     4         0.0267
#> 10 Pride & Prejudice to     4162 122204     2         0.0341
#> # ℹ 40,368 more rows
 
freq_by_rank %>% 
  ggplot(aes(rank, term_frequency, color = book)) + 
  geom_line(linewidth = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()


rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(term_frequency) ~ log10(rank), data = rank_subset)


freq_by_rank %>% 
  ggplot(aes(rank, term_frequency, color = book)) + 
  geom_abline(intercept = -0.62, slope = -1.1, 
              color = "gray50", linetype = 2) +
  geom_line(linewidth = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

book_tf_idf <- book_words %>%
  bind_tf_idf(word, book, n)

book_tf_idf
#> # A tibble: 40,378 × 7
#>    book              word      n  total     tf   idf tf_idf
#>    <fct>             <chr> <int>  <int>  <dbl> <dbl>  <dbl>
#>  1 Mansfield Park    the    6206 160465 0.0387     0      0
#>  2 Mansfield Park    to     5475 160465 0.0341     0      0
#>  3 Mansfield Park    and    5438 160465 0.0339     0      0
#>  4 Emma              to     5239 160996 0.0325     0      0
#>  5 Emma              the    5201 160996 0.0323     0      0
#>  6 Emma              and    4896 160996 0.0304     0      0
#>  7 Mansfield Park    of     4778 160465 0.0298     0      0
#>  8 Pride & Prejudice the    4331 122204 0.0354     0      0
#>  9 Emma              of     4291 160996 0.0267     0      0
#> 10 Pride & Prejudice to     4162 122204 0.0341     0      0
#> # ℹ 40,368 more rows 
 
book_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

library(forcats)

book_tf_idf %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)


library(gutenbergr)
physics <- gutenberg_download(c(37729, 14725, 13476, 30155), 
                              meta_fields = "author")
