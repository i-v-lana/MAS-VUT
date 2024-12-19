library(readr)
library(plotly)
library(reshape2)
# Používáme knihovnu readr, protože v csv souboru se kterým pracujeme je Byte Order Mark,
# který se z nějakého důvodu v R propisuje i do názvu prvního sloupce

# soubor obsahuje data o porodnosti pro kazdý stat od roku 1800 až k roku 2100 (sloupce - "country", "1800", "1801"...)
data = read_csv("children_per_woman_total_fertility.csv", col_names=TRUE)

# zdroj dat uvádí, že všechno, co je po roce 2021 jsou predikce - ty nechceme, proto se omezíme pouze na roky 1800 až 2021
columns <- c("country")
columns <- c(columns, as.character(1800:2021))
actual_data <- data[, columns, drop = FALSE]
actual_data <- actual_data[order(actual_data$country), ]

# změníme strukturu dat - do sloupců dáme názvy států, do řádků - údaje o porodnosti pro káždý rok
formatted_data <- as.data.frame(t(actual_data))
formatted_data <- cbind(index = rownames(formatted_data), formatted_data)
rownames(formatted_data) <- NULL
colnames(formatted_data) <- formatted_data[1, ]
formatted_data <- formatted_data[-1, ]
colnames(formatted_data)[1] <- "year"
formatted_data$year <- as.integer(formatted_data$year)


# podíváme se blíž na vývoj porodnosti ve státech: USA, Indie, Německo, Čína, Česko, JAR, Rusko.
chosen_countries <- c("USA", "India", "Germany", "China", "Czech Republic", "South Africa", "Russia")
plot_data <- formatted_data[, c("year", chosen_countries), drop = FALSE]
long_data <- reshape2::melt(plot_data, id.vars = "year",
                            variable.name = "country", value.name = "value")
long_data$value <- as.numeric(long_data$value)
fig <- plot_ly(long_data,
               x = ~year,
               y = ~value,
               color = ~country,
               type = 'scatter',
               mode = 'lines')
fig

# Ověříme, zda průměrná porodnost v roce 2021 má normální rozdělení po celém světě pomocí Shapiro-Wilkova testu.
# Nulová hypotéza je, že data pocházejí z normálního rozdělení.

data2021 <- formatted_data %>%
  filter(year == 2021) %>%
  select(where(~ all(!is.na(.))))
children2021 <- as.numeric(data2021[1, -1])
children2021 <- sort(children2021)
shapiro_test <- shapiro.test(children2021)

print("Shapiro test results")
shapiro_test

# p-hodnota je 1e-12, takže nulová hypotéza je zamítnuta, což znamená, že data nepocházejí z normálního rozdělení.

# Ověříme totéž, ale pouze pro evropské země.

# Slovensko a Kypr nejsou v csv souboru, takže je nezahrňujeme
eu_countries <- c(
  'Austria', 'Belgium', 'Bulgaria', 'Croatia', 'Czech Republic',
  'Denmark', 'Estonia', 'Finland', 'France', 'Germany', 'Greece', 'Hungary', 'Ireland',
  'Italy', 'Latvia', 'Lithuania', 'Luxembourg', 'Malta', 'Netherlands', 'Poland',
  'Portugal', 'Romania', 'Slovenia', 'Spain', 'Sweden'
)
data2021_eu <- data2021 %>%
  select(all_of(eu_countries))
children2021_eu <- as.numeric(data2021_eu[1, ])
children2021_eu <- sort(children2021_eu)
shapiro_test_eu <- shapiro.test(children2021_eu)
print("Shapiro test on EU")
shapiro_test_eu

# p-hodnota je mnohem vyšší než 0.05, takže data mohou pocházet z normálního rozdělení.

t_test <- t.test(children2021_eu, mu = 2.0)
t_test

# Podívejme se na průměrnou hodnotu plodnosti z průměrů na zemi v Evropě a na směrodatnou odchylku:
mean_value <- mean(children2021_eu)
sd_value <- sd(children2021_eu)
mean_value
sd_value



