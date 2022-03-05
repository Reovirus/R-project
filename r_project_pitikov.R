
Packages <- c("tidyverse", "tseries", "caret", "ggpubr", "stats", "corrplot", "GGally", "rstatix", "colorspace", "sandwich", "lmtest")
lapply(Packages, library, character.only = TRUE)

wine <- read_csv('/Users/pitikov_egor/Desktop/winemag-data-130k-v2.csv')
wine

wine_sub <- subset(wine, select = -c(1, description, designation, province, region_1, region_2, title, variety, winery, taster_twitter_handle))

ggpairs(wine_sub, cardinality_threshold = 45, upper = list(continuous = wrap("cor", method = "spearman")))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4), axis.text.y = element_text(size = 4))

as <- ggplot(wine, aes(x=price, y=points)) + 
  geom_point(aes(color = country)) + 
  stat_smooth(method = lm, se = T) + 
  ylim(75, 101) + 
  xlim(0, 1000)
ns <- ggplot(wine, aes(x=price, y=points)) + 
  geom_point(aes(color = country)) + 
  stat_smooth(method = lm, se = T) + 
  ylim(75, 101) +
  ggtitle("Plot of points by price")
as

a <- cor.test(wine$points, wine$price, method = 'spearman')
a

g1 <- ggplot(wine, aes(price)) +
  geom_histogram(aes(y = after_stat(density)),
                 position = 'identity') + 
  geom_density(bw = 8, alpha = 8) +
  xlim(0, 200) +
  ggtitle("Histogramm of price")
g2 <- ggplot(wine, aes(points)) +
  geom_histogram(aes(y = after_stat(density), binwidth=0.5),
                 position = 'identity') + 
  geom_density(bw = 8, alpha = 8) +
  xlim(70, 101) + 
  scale_x_continuous(breaks = seq(70, 101, by = 5)) +
  ggtitle("Histogramm of scores")

#ggarrange(g1, g2 + rremove("x.text"), 
 #         labels = c("A", "B"),
  #        ncol = 2, nrow = 1, vjust = 10, align = "v", widths = 1, heights = c(4, 4))
g1
g2

x <- as.numeric(wine$points)
x <- x[!is.na(x)]
f <- jarque.bera.test(x)
f

y <- as.numeric(wine$price)
y <- x[!is.na(y)]
q <- jarque.bera.test(y)
q

ggplot(wine, aes(y=points, fill=country)) + 
  geom_boxplot()+
  theme(axis.text.x=element_text(angle =- 90, vjust = 0.5))+
  ggtitle("Boxplot scores to country")
c <- wine %>% drop_na(country) %>% kruskal.test(points ~ country)
c

test_res <- wine %>% dunn_test(points ~ country, p.adjust.method = "holm") %>% select(group1, group2, p.adj)
ggplot(test_res, aes(group1, group2)) + geom_tile(aes(fill = p.adj)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6), axis.text.y = element_text(size = 6), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + ggtitle("Heatmap of Dunn pvals")


c <- wine %>% drop_na(country) %>% kruskal.test(points ~ country)
c

ggplot(wine, aes(y=price, x=taster_name, fill=taster_name)) + 
  geom_boxplot()+
  theme(axis.text.x=element_text(angle =- 90, vjust = 0.5))+
  ylim(0, 250)+
  ggtitle("Boxplot price to taster_name")

test_res <- wine %>% dunn_test(price ~ taster_name, p.adjust.method = "holm") %>% select(group1, group2, p.adj)
ggplot(test_res, aes(group1, group2)) + geom_tile(aes(fill = p.adj)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + ggtitle("Heatmap of Dunn pvals price of tasters")

c <- wine %>% drop_na(taster_name, price) %>% kruskal.test(price ~ taster_name)
c

ggplot(wine, aes(y=points, x=taster_name, fill=taster_name)) + 
  geom_boxplot()+
  theme(axis.text.x=element_text(angle =- 90, vjust = 0.5))+
  ylim(70, 101)+
  ggtitle("Boxplot score to taster_name")
c <- wine %>% drop_na(country) %>% kruskal.test(points ~ taster_name)
c

test_res <- wine %>% dunn_test(points ~ taster_name, p.adjust.method = "holm") %>% select(group1, group2, p.adj)
ggplot(test_res, aes(group1, group2)) + geom_tile(aes(fill = p.adj)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + ggtitle("Heatmap of Dunn pvals points of tasters")

wine %>% drop_na(taster_name, country) %>% group_by(taster_name, country) %>%summarise(count = n())
wine_grouped <- wine %>% drop_na(taster_name, country) %>% group_by(taster_name, country) %>%summarise(count = n())%>% pivot_wider(names_from = taster_name, values_from = count)
wine_grouped[is.na(wine_grouped)] <- 0
f.res <- fisher.test(wine_grouped[2:20], simulate.p.value = TRUE, B = 10000)
f.res

wine$total <- sapply(wine$description, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))
wine

as <- ggplot(wine, aes(x=total, y=points)) + 
  geom_point(aes(color = taster_name)) + 
  stat_smooth(method = lm, se = T) + ggtitle("Plot of points by total words")
as

a <- cor.test(wine$points, wine$total, method = 'spearman')
a

test_res <- wine %>% dunn_test(total ~ taster_name, p.adjust.method = "holm") %>% select(group1, group2, p.adj)
ggplot(test_res, aes(group1, group2)) + geom_tile(aes(fill = p.adj)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6), axis.text.y = element_text(size = 6), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + ggtitle("Heatmap of Dunn pvals of total between somelje")

ggplot(wine, aes(y=total, x=taster_name, fill=taster_name)) + 
  geom_boxplot()+
  theme(axis.text.x=element_text(angle =- 90, vjust = 0.5))+
  ylim(0, 250)

c <- wine %>% drop_na(taster_name) %>% kruskal.test(total ~ taster_name)
c

g1 <- ggplot(wine, aes(total)) +
  geom_histogram(aes(y = after_stat(density)),
                 position = 'identity') + 
  geom_density(bw = 8, alpha = 8) +
  xlim(0, 200) +
  ggtitle("Histogramm of price")
g1
x <- as.numeric(wine$total)
x <- x[!is.na(x)]
f <- jarque.bera.test(x)
f

total_to_points <- lm(points ~ total*taster_name, wine)
summary(total_to_points)
plot(total_to_points, las = 1)
coeftest(total_to_points, vcov = vcovHC(total_to_points, type = 'HC3'))

total_to_points <- lm(points ~ (total + price)*taster_name, wine)
summary(total_to_points)
plot(total_to_points, las = 1)
coeftest(total_to_points, vcov = vcovHC(total_to_points, type = 'HC3'))

libs <- c('tidytext', 'stringr', 'tidyr', 'wordcloud', 'reshape2', 'hunspell','SnowballC', 'xtable', 'knitr', 'kableExtra')
lapply(libs, library, character.only = TRUE)

superwine <- wine %>%
  filter(str_detect(description, "^[^>]+[A-Za-z\\d]") | description !="") 
superwine <- tibble(id_review = as.numeric(superwine$...1) , text_review = superwine$description, is_good = superwine$description >= 92)
superwine <- superwine %>%  unnest_tokens(word, text_review) %>% na.omit()
superwine

superwine %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 20000) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_col() + 
  xlab(NULL) + 
  coord_flip() +
  ggtitle("Plot top words count")


superwine_counts <- superwine %>% group_by( word, is_good) %>%summarise (count = n())
superwine_counts %>% 
  group_by(is_good) %>% 
  top_n(25, count) %>%
  ungroup() %>%
  mutate(word = reorder(word, count)) %>%
  ggplot(aes(y = word, x = count, fill = is_good)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~is_good, scales = "free_y") + 
  labs(y = "Contribution to Sentiment", x = NULL) +
  ggtitle("Plot top words into categories")


superwine %>% 
  count(word, is_good, sort = TRUE) %>% 
  acast(word ~ is_good, value.var = "n", fill = 0) %>% 
  comparison.cloud(colors = c("gray20", "gray80"), max.words = 100)

most_freq <- superwine_counts %>% 
  group_by(is_good) %>% 
  top_n(100, count) %>%
  ungroup()
most_freq <- most_freq %>% distinct(word)
pval_freqs <- c()
for (pattern in most_freq$word){
  my_mat <- c(filter(superwine_counts, word==pattern & is_good == T)[1, 'count'],
    filter(superwine_counts, word==pattern & is_good == F)[1, 'count'],
    sum(filter(superwine_counts, word!=pattern & is_good == T)[, 'count']),
    sum(filter(superwine_counts, word!=pattern & is_good == F)[, 'count']))
  my_mat <- unlist(my_mat, use.names=FALSE)
  my_mat <- matrix(my_mat,nrow=2,ncol=2,byrow=TRUE)
  my_mat[is.na(my_mat)] = 0
  cur_pva <- fisher.test(my_mat, simulate.p.value = T, B = 10000)$p.value
  pval_freqs <- append(pval_freqs, cur_pva)
}

pval_freqs <- p.adjust(pval_freqs, method = "holm")
pval_table <- data.frame(word = most_freq$word, b = -1*log(pval_freqs, base = 2))
pval_table <- as_tibble(pval_table)

most_freq <- superwine_counts %>% 
  group_by(is_good) %>% 
  top_n(100, count) %>%
  ungroup()
pval_table = left_join(pval_table, most_freq, by = c("word" = "word"))

pval_table %>%
  top_n(50, b) %>%
  ggplot(aes(y = word, x = b, fill = is_good)) + 
  geom_col(show.legend = T) + 
  labs(y = "", x = NULL) +
  ggtitle("-log2 pvalues") +
  ggtitle("Plot of top words pvalues")
