
library(dplyr)
# load data

full_data = read_excel("~/Downloads/bank_data.xlsx") %>%
  gather(date, close_price, -ticker) %>%
  group_by(ticker) %>%
  mutate(date = as.Date(date),
         close_price = as.numeric(close_price)) %>%
  arrange(ticker, date) %>%
  group_by(ticker) %>%
  filter(ticker != "ABBB") %>%
  mutate(daily_return = (close_price/lag(close_price)) - 1) %>%
  mutate(daily_return_ann = (1+daily_return)^{365} - 1) %>%
  filter(!is.na(daily_return ))

# bank_balance_sheet = read_excel("~/Downloads/bank_balance_sheets.xlsx")  %>%
#   mutate(date = as.Date(`Data Date`)) %>%
#   select(date, ticker = `Ticker Symbol`,
#          deposit = `Deposits - Total`,
#          assets = `Assets - Total`,
#          liabilities = `Liabilities - Total`,
#          fhlb = `Advances from FHLB`) %>%
#   mutate(fhlb = replace_na(fhlb, 0)) %>%
#   filter(date == as.Date("2022-12-31")) %>%
#   select(-date)

bank_data = full_data %>% filter(ticker != "s&p")
sp_data = full_data %>% filter(ticker == "s&p") %>%
  ungroup() %>%
  select(daily_return_mkt = daily_return, date)

bank_data = bank_data %>% left_join(sp_data) %>%
  mutate(abnormal = daily_return - daily_return_mkt) %>%
  mutate(cumul_abnormal = exp(cumsum(log(1+abnormal)))-1) %>%
  filter(ticker != "SI")  

bank_cumul_ret = bank_data %>% filter(date == as.Date("2023-03-10")) %>%
  arrange(cumul_abnormal) 
ggplot(data= bank_cumul_ret[1:20,]) +
  geom_col(aes(y = cumul_abnormal, x= reorder(ticker, cumul_abnormal))) + 
  coord_flip() +
  theme_minimal() +
  labs(y = "Cumulative Return Excess of Market", x = "Bank Ticker") +
  scale_y_continuous(labels = scales::percent_format()) 

ggplot(data= bank_cumul_ret) +
  geom_density(aes(x = cumul_abnormal)) + 
  theme_minimal() +
  labs(y = "Percent of Banks", x = "Cumulative Return Excess of Market") +
  scale_x_continuous(labels = scales::percent_format()) 
# 
# ggplot(data= bank_cumul_ret %>% left_join(bank_balance_sheet) %>% 
#          filter(!is.na(deposit))) +
#   geom_point(aes(x = deposit, y = cumul_abnormal )) + 
#   geom_smooth(aes(x = deposit, y = cumul_abnormal )) + 
#   theme_minimal() +
#   labs(x = "Total Deposits (millions)", y = "Cumulative Return Excess of Market") +
#   scale_x_log10(labels = scales::dollar_format()) +
#   scale_y_continuous(labels = scales::percent_format())

 bank_list = c( bank_cumul_ret[1:5,] %>% pull(ticker), "JPM", "C", "BAC", "WFC")

 bank_list = c( bank_cumul_ret[1:7,] %>% pull(ticker))
ggplot(data = bank_data %>% 
         filter(ticker %in% bank_list)) + 
  geom_line(aes(y = cumul_abnormal, x = date, color = ticker)) +
  geom_point(aes(y = cumul_abnormal, x = date, color = ticker)) +
  geom_text_repel(data = bank_data %>% 
              filter(ticker %in% bank_list & date == as.Date("2023-03-10")), 
            aes(y = cumul_abnormal, x = date, label = ticker, color = ticker),
            hjust=0
            ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_date(limits = c(as.Date("2023-03-02"), as.Date("2023-03-11")))+
  labs(y = "Cumulative Return Excess of Market",
       x = "Date") +
  theme_minimal() +
  theme(legend.position = "none")


