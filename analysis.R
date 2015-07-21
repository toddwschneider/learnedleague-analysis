library(ggplot2)
library(scales)
library(dplyr)
library(reshape2)
library(readr)
library(rpart)
library(rpart.plot)
library(grid)
source("helpers.R")

stats = read_csv("learnedleague_category_stats.csv")

correct_rates = dcast(stats, anon_id + gender + overall_correct_pct ~ category, value.var = "ratio")
outperformance = dcast(stats, anon_id + gender + overall_correct_pct ~ category, value.var = "ratio_relative_to_overall")

categories = sort(unique(stats$category))

display_names = c(
  amer_hist = "American History",
  art = "Art",
  bus_econ = "Business/Economics",
  class_music = "Classical Music",
  curr_events = "Current Events",
  film = "Film",
  food_drink = "Food/Drink",
  games_sport = "Games/Sport",
  geography = "Geography",
  language = "Language",
  lifestyle = "Lifestyle",
  literature = "Literature",
  math = "Math",
  pop_music = "Pop Music",
  science = "Science",
  television = "Television",
  theatre = "Theatre",
  world_hist = "World History"
)

# calculate correlations
# assumes input_data is a data frame that has a column for each category
calculate_correlation_pairs = function(input_data) {
  correlation_matrix = cor(input_data[, categories])
  rownames(correlation_matrix) = display_names[rownames(correlation_matrix)]
  colnames(correlation_matrix) = display_names[colnames(correlation_matrix)]

  correlation_pairs = melt(correlation_matrix)
  names(correlation_pairs) = c("category_1", "category_2", "rho")
  correlation_pairs = filter(correlation_pairs, as.character(category_1) < as.character(category_2))
  correlation_pairs = correlation_pairs[rev(order(correlation_pairs$rho)), ]

  correlation_pairs
}

correct_rates_correlations = calculate_correlation_pairs(correct_rates)
correct_rates_correlations$type = "absolute correct rate"

outperformance_correlations = calculate_correlation_pairs(outperformance)
outperformance_correlations$type = "relative to individual overall average"

write_csv(rbind(correct_rates_correlations, outperformance_correlations), "correlation_pairs.csv")


# scatterplots of category pairs
plot_categories = function(x, y) {
  p = ggplot(data = correct_rates, aes_string(x = x, y = y)) +
    geom_point(color = "#900000", alpha = 0.15, size = 4.5) +
    geom_smooth(method = "lm", se = FALSE) +
    scale_x_continuous(paste0("\n", display_names[x], " questions correct %"), labels = percent) +
    scale_y_continuous(paste0(display_names[y], " questions correct %\n"), labels = percent) +
    title_with_subtitle(paste0(display_names[y], " vs. ", display_names[x]),
                        paste0("Based on trivia results from ", nrow(correct_rates), " LearnedLeague players")) +
    theme_tws(base_size = 18) +
    theme(plot.margin = unit(c(1, 1, 1.25, 0.5), "lines"))

  p
}

apply(expand.grid(categories, categories), 1, function(row) {
  x = row[1]
  y = row[2]
  filename = paste0("graphs/", x, "__", y, ".png")

  png(filename = filename, width = 480, height = 480)
  print(plot_categories(x, y))
  add_credits(fontsize = 10)
  dev.off()
})

# barplots for categories
category_barplot = function(category) {
  data = filter(correct_rates_correlations, category_1 == category | category_2 == category)
  other = data$category_1
  other[which(other == category)] = data$category_2[which(other == category)]
  data$other = as.character(other)
  data = data[rev(order(data$rho)), ]
  data$other = factor(data$other, levels = data$other)

  p = ggplot(data = data, aes(x = other, y = rho)) +
    geom_bar(stat = "identity", fill = "#120060") +
    scale_y_continuous("correlation\n", lim = c(0, 1)) +
    scale_x_discrete("") +
    labs(title = paste0(category, " correlation to other categories\n")) +
    theme_tws(base_size = 18) +
    theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0.05),
          legend.position = "none")

  p
}

for(category in categories) {
  display_name = display_names[category]
  png(filename=paste0("graphs/", category, "_barplot.png"), width=640, height=480)
  print(category_barplot(display_name))
  add_credits(fontsize = 10)
  dev.off()
}

# gender classification tree
set.seed(1738)
model_data = filter(correct_rates, gender %in% c("Male", "Female"))[, c("gender", categories)]
names(model_data) = c("Gender", display_names)
rpart_model = rpart(Gender ~ ., data = model_data, parms = list(prior = c(0.5, 0.5)))

printcp(rpart_model)
plotcp(rpart_model)

# prune tree based on CP table
pruned_model = prune(rpart_model, cp = 0.014)

table(model_data$Gender, predict(rpart_model, type = "class"))
table(model_data$Gender, predict(pruned_model, type = "class"))

pruned_model$variable.importance

png(filename = "graphs/gender_decision_tree.png", width = 640, height = 1080)
par(bg = "#f4f4f4")
prp(rpart_model,
    type = 3, extra = 8,
    main = "Gender classification tree based on category performance\nin LearnedLeague trivia competition",
    clip.right.labs = FALSE, branch = 1, varlen = 0,
    cex = 1.1, cex.main = 1.7,
    box.col = c("pink", "skyblue")[rpart_model$frame$yval])
add_credits(fontsize = 14)
dev.off()

png(filename = "graphs/pruned_gender_decision_tree.png", width = 640, height = 720)
par(bg = "#f4f4f4")
prp(pruned_model,
    type = 3, extra = 8,
    main = "Gender classification tree based on category performance\nin LearnedLeague trivia competition",
    clip.right.labs = FALSE, branch = 1, varlen = 0,
    cex = 1.1, cex.main = 1.7,
    box.col = c("pink", "skyblue")[pruned_model$frame$yval])
add_credits(fontsize = 14)
dev.off()

# ranking the categories
gender_stats = summarize(
  group_by(filter(stats, gender %in% c("Male", "Female")),
           gender, category),
  outperformance = sum(ratio_relative_to_overall * total) / sum(total)
)

gender_stats$category = display_names[gender_stats$category]

gender_diffs = summarize(
  group_by(gender_stats, category),
  outperformance_diff = sum(outperformance * (gender == "Male")) - sum(outperformance * (gender == "Female"))
)

gender_diffs = gender_diffs[order(gender_diffs$outperformance_diff), ]
gender_diffs$category = factor(gender_diffs$category, levels = gender_diffs$category)

png(filename = "graphs/category_preferences.png", width = 640, height = 640)
ggplot(data = gender_diffs, aes(x = category, y = outperformance_diff, fill = factor(sign(outperformance_diff)))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous("(male - female) preference\n", labels = percent) +
  scale_x_discrete("") +
  scale_fill_manual("", values = c("pink", "skyblue")) +
  labs(title = "LearnedLeague trivia categories ranked\nby Male/Female preference\n") +
  theme_tws(base_size = 18) +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 0.05),
        legend.position = "none")
add_credits(fontsize = 14)
dev.off()

# logistic regression
summary(glm(factor(Gender) ~ ., data = model_data, family = binomial(link = "logit")))
