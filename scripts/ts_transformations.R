library(readr)
library(dplyr)
# for panel data
library(tibble)
# for ts data
library(tsibble)
# allows you to erxtract parts of dates
library(lubridate)
# to use ggplot
library(ggplot2)
library(feasts)

######### CRIME ##########
crime <- readr::read_csv("../data/baltimore_crime.csv")

# aggregate by days and convert to tibble
tb_crime <- tibble(crime) |>
  select(CrimeDate) |>
  group_by(CrimeDate) |>
  summarise(total = n()) |>
  arrange(CrimeDate) |>
  mutate(CrimeDate = as_date(CrimeDate, format="%m/%d/%Y")) |>
  filter(between(year(CrimeDate),2011,2015))
  
ts_crime <- tb_crime |> as_tsibble(index=CrimeDate)

######## Monthly Total ##########
ts_crime_monthly <- ts_crime |>
  index_by(Month = ~ floor_date(.x, "month")) |>
  filter(between(year(Month),2011,2015)) |>
  summarise(Monthly_Total = sum(total))

ts_crime_monthly

######## Average Monthly ########
ts_crime_monthly_avg <- ts_crime |>
  index_by(Month = ~ floor_date(.x, "month")) |>
  filter(between(year(Month),2011,2015)) |>
  summarise(Monthly_Total = sum(total)) |>
  mutate(Num_Days = days_in_month(Month), 
         Avg_perMonth = Monthly_Total/Num_Days)

y_left  <- ts_crime_monthly$Monthly_Total
y_right <- ts_crime_monthly_avg$Avg_perMonth

#grey <- rgb(0, 0, 0, alpha = 0.4)
grey = "#c0c0c0"
pink = "#ea3788"

twoord.plot(lx = x_numeric, 
            ly = y_left, 
            rx = x_numeric, 
            ry = y_right,
            main = "Baltimor Crime",
            type = "l",
            lcol = grey, 
            rcol = pink, 
            mar = c(5,4,4,4),
            xtickpos = x_numeric, 
            xticklab = format(x_dates, "%b %Y"),
            lwd = 4
            )

legend("bottomleft", 
       bty="n",
       legend = c("Total Crimes/Month", "Average Crimes/Month"), 
       col = c(grey, pink), 
       lty = 1, # Line type
       lwd = 3
) # Line width

###### AUTOMOBILES ######
auto <- readr::read_csv('../data/us_car_reg.csv', col_names = c('year','total'))
us_pop <- readr::read_csv('../data/us_pop.csv', col_names = c('year','total'))

auto_tsb <- tsibble(auto, index = year)
us_pop_tsb <- tsibble(us_pop, index = year)

auto_tsb |> autoplot()

auto_tsb <- auto_tsb |> 
  mutate(cars_per_1000 = (auto_tsb$total/us_pop_tsb$total)*1000)

auto_pop <- left_join(auto_tsb, us_pop_tsb, by="year")

auto_pop <- auto_pop |> 
  rename( auto = total.x, population = total.y) |>
  mutate(cars_per_1000 = (auto/population)*1000)
  
blue = "#3a6ea5"
orange = "#f15025"

twoord.plot(lx = auto_pop$year, 
            ly = auto_pop$auto, 
            rx = auto_pop$year, 
            ry = auto_pop$cars_per_1000,
            main = "Automobile ownership in the US",
            type = "l",
            lcol = grey, 
            rcol = blue,
            lwd = 4
)

legend("topright", 
       bty="n",
       legend = c("Total Automobiles", "Automobiles/1000 ppl"), 
       col = c(grey, blue), 
       lty = 1, # Line type
       lwd = 3  # Line width
       ) 
  
  #### CPI ####
library(readr)

# load dataset and adjust for inflation 
watch_sales <- readr::read_csv("../data/watch_sales.csv") |>
  mutate(real = (nominal/cpi)*240.01)

# data transformation for plotting
pl_watch_sales <- watch_sales |> 
  # unpivot data from columns to rows
  pivot_longer(c(nominal, real), 
               values_to="amount") |>
  # convert variables from <chr> to categorical
  mutate(name = factor(name,      
                       levels=c("nominal","real")))
blue = "#3a6ea5"
orange = "#f15025"
grey = "#dfe0e1"

# plot the graph
pl_watch_sales |> ggplot(aes(x=year, y=amount)) +
  geom_line(size = 1.3,                                        # line width
            colour = blue) + 
  geom_smooth(method = "lm", se = FALSE, colour = orange) +
  facet_grid(name ~ ., scales = "free_y") +
  labs(title = "Handwatch Industry Revenue for 2017-2023",
       x="",
       y="Revenue (in billions)") +
  theme(
  # change plot background  
  panel.background = element_rect(fill = NA),
  # change Y-grid color
  panel.grid.major.y = element_line(colour = grey),
  # hide horizontal background grid
  panel.grid.major.x = element_line(colour = NA),
  # change title font size here
  plot.title = element_text(size = 15),
  # set background
  plot.background = element_rect(color = NA)
)

  
