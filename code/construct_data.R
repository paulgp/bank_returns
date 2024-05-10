setwd("~/Dropbox/bank_returns")
library(tidyverse)
library(ggrepel)
library(fixest)
library(data.table)


date_list <- c(
  as.Date("2023-03-17"),
  as.Date("2023-05-25")
)
early_date <- date_list[1]
late_date <- date_list[2]
# Load Data ----
## Ticker - RSSDID - PermCo Crosswalk
permco_rssdid_xwalk <- fread("data/input/permco_rssdid_xwalk.csv") %>% filter(!is.na(entity))
wrds_compustat_data <- fread("data/input/wrds_data/wrds_2022data.csv")

### LUTHER BURBANK CORP and SOUTH PLAINS FINANCIAL INC show up twice
### For Luther, we use the BHC RSSDID 3814208
### For South Plains, we use RSSDID 2033226 b/c the other RSSDID is a stock plan according to the NIC
bank_sample <- permco_rssdid_xwalk %>%
  inner_join(wrds_compustat_data, by = c("permco" = "PERMCO")) %>%
  filter(dt_end >= 20210930) %>%
  filter(!(entity == 497570 | entity == 3382332))

## Y9C Data ----
y9c_data_2022q4 <- fread("data/input/y9c_data/BHCF20211231.txt", sep = "^")
y9c_data_2022q4_subsample <- bank_sample %>%
  inner_join(
    y9c_data_2022q4 %>%
      mutate(
        cash_securities = rowSums(
          select(
            .,
            BHCK0081, BHCK0395, BHCK0397, BHCKJJ34, BHCK1773, BHCKJA22
          ),
          na.rm = TRUE
        ),
        cash = rowSums(
          select(
            .,
            BHCK0081, BHCK0395, BHCK0397
          ),
          na.rm = TRUE
        ),
        securities = rowSums(
          select(
            .,
            BHCKJJ34, BHCK1773, BHCKJA22
          ),
          na.rm = TRUE
        ),
        liabilties = BHCK2948,
        dep_y9c = rowSums(
          select(
            .,
            BHDM6631, BHDM6636
          ),
          na.rm = TRUE
        ),
        nonperforming_loans = rowSums(
          select(
            .,
            BHCK1407, BHCK1403
          ),
          na.rm = TRUE
        ),
        total_loans = rowSums(
          select(
            .,
            BHCK5369, BHCKB528
          ),
          na.rm = TRUE
        ),
        deposits = as.numeric(BHDM6631) + as.numeric(BHDM6636) +
          as.numeric(BHFN6631) + as.numeric(BHFN6636),
        tier1capratio = coalesce(BHCA7206, BHCW7206),
        htm = BHCKJJ34,
        unrealized_htm_losses = BHCK1754 - BHCK1771,
        htm_book = BHCK1754,
        assets = BHCK2170,
        tier1_capital = BHCA8274,
        afs = BHCK1773
      ) %>%
      select(
        deposits, tier1_capital, BHCA8274, cash_securities,
        unrealized_htm_losses, cash, assets, afs,
        htm, htm_book, securities, liabilties, dep_y9c,
        RSSD9001, nonperforming_loans, total_loans,
        tier1capratio
      ),
    by = c("entity" = "RSSD9001")
  ) %>%
  mutate(
    htm_ratio = htm / assets,
    htm_ratio2 = htm / afs,
    npl_ratio = nonperforming_loans / total_loans,
    unrealized_htm_losses_rat_book = (unrealized_htm_losses) / htm_book,
    unrealized_htm_losses_rat_tier1 = (unrealized_htm_losses) / tier1_capital,
    liquid_asset_ratio = cash_securities / assets,
    liquid_asset_ratio2 = cash_securities / tier1_capital,
    cash_asset_ratio = cash / assets,
    securities_asset_ratio = securities / assets,
    loans_asset_ratio = total_loans / assets,
    deposits_liabilities_ratio = deposits / liabilties
  ) %>%
  filter(!is.na(assets))

## Call Report Data ----
files <- dir("data/input/FFIEC CDR Call Bulk All Schedules 12312022/", pattern = ".txt", full.names = TRUE)

call_report_data_vars <- fread(files[1])
for (fn in files[2:48]) {
  call_report_data_vars <- call_report_data_vars %>% left_join(fread(fn, fill = TRUE),
    suffix = c("", ".y"),
    by = c("IDRSSD")
  )
}
call_report_subsample_2022q4 <- bank_sample %>%
  filter(dt_end >= 20210930) %>%
  inner_join(
    call_report_data_vars %>%
      mutate(
        cash = coalesce(RCFD0081 + RCFD0071, RCON0081 + RCON0071),
        securities = coalesce(RCFDJJ34 + RCFD1773 + RCFDJA22, RCONJJ34 + RCON1773 + RCONJA22, ),
        cash_securities = cash + securities,
        liabilties = coalesce(RCFD2948, RCON2948),
        nonperforming_loans = coalesce(RCFD1407 + RCFD1403, RCON1407 + RCON1403),
        total_loans = coalesce(RCFD5369 + RCFDB528, RCON5369 + RCONB528),
        htm = coalesce(RCFDJJ34, RCONJJ34),
        unrealized_htm_losses = coalesce(RCFD1754, RCON1754) - coalesce(RCFD1771, RCON1771),
        htm_book = coalesce(RCFD1754, RCON1754),
        assets = coalesce(RCFD2170, RCON2170),
        tier1_capital = coalesce(RCFA8274, RCOA8274),
        afs = coalesce(RCFD1773, RCON1773)
      ) %>%
      select(
        tier1_capital, cash_securities,
        unrealized_htm_losses, cash, assets, afs,
        htm, htm_book, securities, liabilties,
        IDRSSD, nonperforming_loans, total_loans
      ),
    by = c("entity" = "IDRSSD")
  ) %>%
  mutate(
    htm_ratio = htm / assets,
    htm_ratio2 = htm / afs,
    npl_ratio = nonperforming_loans / total_loans,
    unrealized_htm_losses_rat_book = (unrealized_htm_losses) / htm_book,
    unrealized_htm_losses_rat_tier1 = (unrealized_htm_losses) / tier1_capital,
    tier1capratio_cr = tier1_capital / assets,
    liquid_asset_ratio = cash_securities / assets,
    liquid_asset_ratio2 = cash_securities / tier1_capital,
    cash_asset_ratio = cash / assets,
    securities_asset_ratio = securities / assets,
    loans_asset_ratio = total_loans / assets
  ) %>%
  filter(!is.na(assets))

## FDIC Deposit Data ----
### Roll up subsidiaries ----
relationship_data_all <- fread("data/input/y9c_data/CSV_RELATIONSHIPS.csv") %>%
  filter(DT_END > 20230101)

relationship_data <- bank_sample %>%
  select(entity) %>%
  inner_join(relationship_data_all,
    by = c("entity" = "ID_RSSD_PARENT")
  ) %>%
  select(ID_RSSD_PARENT = entity, ID_RSSD_OFFSPRING) %>%
  mutate(top_holder = ID_RSSD_PARENT)

child_relationship_data <- relationship_data %>%
  select(entity = ID_RSSD_OFFSPRING, top_holder) %>%
  inner_join(relationship_data_all,
    by = c("entity" = "ID_RSSD_PARENT")
  ) %>%
  select(ID_RSSD_OFFSPRING, top_holder)
relationship_data <- relationship_data %>%
  select(top_holder, ID_RSSD_OFFSPRING) %>%
  bind_rows(child_relationship_data)
for (i in seq(1, 25)) {
  child_relationship_data <- child_relationship_data %>%
    select(entity = ID_RSSD_OFFSPRING, top_holder) %>%
    inner_join(relationship_data_all,
      by = c("entity" = "ID_RSSD_PARENT")
    ) %>%
    select(ID_RSSD_OFFSPRING, top_holder)
  dim(child_relationship_data)
  relationship_data <- relationship_data %>%
    select(top_holder, ID_RSSD_OFFSPRING) %>%
    bind_rows(child_relationship_data)
}

relationship_data <- relationship_data %>%
  bind_rows(
    bank_sample %>%
      select(top_holder = entity) %>%
      mutate(ID_RSSD_OFFSPRING = top_holder)
  )

call_report_data <- fread("data/input/callreport/CSV_ATTRIBUTES_ACTIVE.csv") %>%
  select(ID_FDIC_CERT, ID_RSSD) %>%
  bind_rows(fread("data/input/callreport/CSV_ATTRIBUTES_CLOSED.csv") %>%
    select(ID_FDIC_CERT, ID_RSSD))

# All banks that show up in the relationship file
children_cert_num <- call_report_data %>%
  inner_join(relationship_data %>% select(top_holder, ID_RSSD_OFFSPRING),
    by = c("ID_RSSD" = "ID_RSSD_OFFSPRING")
  ) %>%
  select(ID_RSSD, ID_RSSD_PARENT = top_holder, fdic_cert_num = ID_FDIC_CERT)

# write to file for FDIC pull
## Run Python Code after this to get bank deposit data
children_cert_num %>%
  filter(fdic_cert_num != 0) %>%
  write_csv("data/input/fdic/cert_nums.csv")

# load FDIC deposit data
fdic_deposit_data <- fread("data/input/fdic/bank_deposits.csv") %>%
  group_by(ID_RSSD_PARENT) %>%
  summarize(
    DEP = sum(DEP),
    DEPINS = sum(DEPINS),
    ASSETS_FDIC = sum(ASSETS)
  ) %>%
  inner_join(bank_sample %>% select(entity, TICKER), by = c("ID_RSSD_PARENT" = "entity"))

## Final FRB-Call Report-FDIC Data ----
# final bank balance sheet data (y9c + call report)
# 224 banks
bank_balance_sheet_sample <- y9c_data_2022q4_subsample %>%
  bind_rows(call_report_subsample_2022q4) %>%
  mutate(tier1capratio = coalesce(tier1capratio / 100, tier1capratio_cr))

# final bank data (data from 2022q4) (FRB + FDIC)
# 224 banks
# THese three BHCs get lost (don't have FDIC data for some reason)
# EMCLAIRE FINANCIAL CORP 1480944
# PCSB FINANCIAL CORP 5079476
# PROFESSIONAL HOLDING CORP. (NASDAQGS:PFHD) 4647494
final_bank_data <- fdic_deposit_data %>%
  inner_join(bank_balance_sheet_sample,
    by = c("TICKER" = "TICKER")
  ) %>%
  mutate(
    assets_imp = coalesce(assets, ASSETS_FDIC),
    dep_unins_share = (1 - DEPINS / DEP)
  )


bank_data_ticker_list <- final_bank_data %>%
  select(ticker = TICKER) %>%
  pull(ticker)
## Stock Market Returns Data ----
returns_data <- fread("data/input/yahoo/returns.csv") %>%
  gather(ticker, close_prc, -Date) %>%
  mutate(Date = as.Date(Date))

returns_data_may25 <- fread("data/input/yahoo/returns_longrun.csv") %>%
  gather(ticker, close_prc_may25, -Date) %>%
  mutate(Date = as.Date(Date))

returns_data <- returns_data_may25 %>%
  left_join(returns_data) %>%
  mutate(close_prc_new = coalesce(close_prc, close_prc_may25)) %>%
  select(-close_prc, -close_prc_may25) %>%
  rename(close_prc = close_prc_new) %>%
  filter(ticker != "SI" & ticker %in% bank_data_ticker_list) %>%
  mutate(close_prc = case_when(
    ticker == "FRC" & Date >= as.Date("2023-05-01") ~ 0,
    ticker == "SBNY" & Date >= as.Date("2023-03-12") ~ 0,
    ticker == "SIVB" & Date >= as.Date("2023-03-10") ~ 0,
    TRUE ~ close_prc
  ))



### S&P 500 Data
sp_data <- read_csv("data/input/yahoo/sp_returns.csv") %>%
  rename(close_prc = `Adj Close**`) %>%
  mutate(Date = as.Date(Date))


### Dow Jones Bank Index
library(readxl)
dowjonesbankindex <- read_excel("data/input/dowjonesbankindex.xlsx") %>%
  mutate(
    Date = as.Date(`Effective date`),
    ticker = "DJBANK"
  ) %>%
  rename(close_prc = `Dow Jones U.S. Banks Total Return Index`) %>%
  arrange(ticker, Date) %>%
  mutate(daily_return_bankindex = (close_prc / lag(close_prc)) - 1) %>%
  ungroup() %>%
  select(Date, daily_return_bankindex)



### calculate returns ----

cutoff_date <- as.Date("2023-02-01")
sp_data_parsed <- sp_data %>%
  group_by(ticker) %>%
  arrange(ticker, Date) %>%
  mutate(
    daily_return_mkt =
      (close_prc / lag(close_prc)) - 1
  ) %>%
  ungroup() %>%
  select(Date, daily_return_mkt) %>%
  filter(!is.na(daily_return_mkt)) %>%
  left_join(dowjonesbankindex, by = "Date") %>%
  filter(Date >= cutoff_date & Date <= late_date) %>%
  mutate(abnormal_bank_idx = daily_return_bankindex - daily_return_mkt) %>%
  mutate(
    cumul_mkt_ret = exp(cumsum(log(1 + daily_return_mkt))) - 1,
    cumul_bankidx_ret = exp(cumsum(log(1 + daily_return_bankindex))) - 1,
    cumul_abnormal_bankidx = exp(cumsum(log(1 + daily_return_bankindex))) - 1
  )


returns_data_parsed <- returns_data %>%
  inner_join(bank_sample %>%
    select(ticker = TICKER)) %>%
  left_join(sp_data_parsed, by = "Date") %>%
  group_by(ticker) %>%
  arrange(ticker, Date) %>%
  filter(Date >= cutoff_date & Date <= late_date) %>%
  fill(close_prc) %>% # replace NA with previous day's close price
  mutate(daily_return = (close_prc / lag(close_prc)) - 1) %>%
  mutate(daily_return = replace_na(daily_return, 0)) %>%
  filter(Date != as.Date("2023-02-01")) %>%
  mutate(abnormal = daily_return - daily_return_mkt) %>%
  mutate(cumul_abnormal = exp(cumsum(log(1 + abnormal))) - 1) %>%
  # mutate(cumul_abnormal2 = cumsum(abnormal)) %>%
  mutate(cumul_ret = exp(cumsum(log(1 + daily_return))) - 1) %>%
  mutate(cumul_abnormal = cumul_ret - cumul_mkt_ret)
# mutate(cumul_abnormal = cumsum(abnormal, na.rm = TRUE))
# utate(cumul_abnormal = replace_na(cumul_abnormal, -1))



sample_bank_return <- returns_data_parsed %>%
  left_join(final_bank_data %>%
    select(assets_imp, ticker = TICKER)) %>%
  group_by(Date) %>%
  mutate(assets_imp = as.numeric(assets_imp)) %>%
  summarize(cumul_ret = weighted.mean(cumul_ret, assets_imp))


# Construct measure of excess returns from Feb 2022 to Feb 2023
sp_data_parsed_2022 <- sp_data %>%
  group_by(ticker) %>%
  arrange(ticker, Date) %>%
  mutate(
    daily_return_mkt =
      (close_prc / lag(close_prc)) - 1
  ) %>%
  ungroup() %>%
  select(Date, daily_return_mkt) %>%
  filter(!is.na(daily_return_mkt)) %>%
  left_join(dowjonesbankindex, by = "Date") %>%
  filter(Date > as.Date("2022-02-01") & Date < cutoff_date) %>%
  mutate(abnormal_bank_idx = daily_return_bankindex - daily_return_mkt) %>%
  mutate(
    cumul_mkt_ret = exp(cumsum(log(1 + daily_return_mkt))) - 1,
    cumul_bankidx_ret = exp(cumsum(log(1 + daily_return_bankindex))) - 1,
    cumul_abnormal_bankidx = exp(cumsum(log(1 + daily_return_bankindex))) - 1
  )

returns_data_parsed_2022 <- returns_data %>%
  inner_join(bank_sample %>%
    select(ticker = TICKER)) %>%
  left_join(sp_data_parsed_2022, by = "Date") %>%
  group_by(ticker) %>%
  arrange(ticker, Date) %>%
  fill(close_prc) %>% # replace NA with previous day's close price
  filter(Date >= as.Date("2022-02-01") & Date < cutoff_date) %>%
  mutate(daily_return = (close_prc / lag(close_prc)) - 1) %>%
  filter(Date != as.Date("2022-02-01")) %>%
  mutate(abnormal = daily_return - daily_return_mkt) %>%
  mutate(cumul_abnormal = exp(cumsum(log(1 + abnormal))) - 1) %>%
  # mutate(cumul_abnormal2 = cumsum(abnormal)) %>%
  mutate(cumul_ret = exp(cumsum(log(1 + daily_return))) - 1) %>%
  mutate(cumul_abnormal = cumul_ret - cumul_mkt_ret)
# mutate(cumul_abnormal = cumsum(abnormal))
# mutate(cumul_abnormal = replace_na(cumul_abnormal, -1))

sample_bank_return_2022 <- returns_data_parsed_2022 %>%
  left_join(final_bank_data %>%
    select(assets_imp, ticker = TICKER)) %>%
  group_by(Date) %>%
  mutate(assets_imp = as.numeric(assets_imp)) %>%
  summarize(
    cumul_ret = weighted.mean(cumul_ret, assets_imp),
    cumul_ret_unw = mean(cumul_ret)
  )



### Construct betas
ticker_betas <- as_tibble(returns_data_parsed_2022) %>%
  filter(year(Date) == 2022) %>%
  select(ticker, daily_return, daily_return_mkt) %>%
  nest(data = -ticker) %>%
  mutate(
    linMod = map(data, ~ lm(daily_return ~ daily_return_mkt, data = .)),
    coef = map(linMod, coefficients),
    slope = map_dbl(coef, 2)
  ) %>%
  select(ticker, beta = slope)

ticker_bankbetas <- as_tibble(returns_data_parsed_2022) %>%
  filter(year(Date) == 2022) %>%
  select(ticker, daily_return, daily_return_mkt, abnormal_bank_idx) %>%
  nest(data = -ticker) %>%
  mutate(
    linMod = map(data, ~ lm(daily_return ~ daily_return_mkt + abnormal_bank_idx, data = .)),
    coef = map(linMod, coefficients),
    vcov = map(linMod, vcov),
    slope = map_dbl(coef, 3),
    slope_se = sqrt(map_dbl(vcov, 9))
  ) %>%
  select(ticker,
    beta_bank = slope,
    # beta_bank_se = slope_se
  )


returns_data_parsed_feb15 <- returns_data_parsed %>%
  filter(Date >= as.Date("2023-02-15")) %>%
  left_join(ticker_betas, by = "ticker") %>%
  left_join(ticker_bankbetas, by = "ticker") %>%
  mutate(abnormal_capm = daily_return - (daily_return_mkt * beta)) %>%
  mutate(cumul_abnormal_capm = exp(cumsum(log(1 + abnormal_capm))) - 1)
returns_data_parsed <- returns_data_parsed %>%
  left_join(ticker_betas, by = "ticker") %>%
  left_join(ticker_bankbetas, by = "ticker") %>%
  mutate(abnormal_capm = daily_return - (daily_return_mkt * beta)) %>%
  mutate(cumul_abnormal_capm = exp(cumsum(log(1 + abnormal_capm))) - 1)

bank_cumul_ret_multiple <- returns_data_parsed %>%
  group_by(ticker) %>%
  # filter(row_number() == n())  %>%
  filter(Date >= as.Date("2023-03-03") &
    Date <= as.Date("2023-04-30")) %>%
  arrange(cumul_abnormal) %>%
  filter(Date %in% date_list)

bank_cumul_ret_early <- returns_data_parsed %>%
  group_by(ticker) %>%
  # filter(row_number() == n())  %>%
  filter(Date == early_date) %>%
  arrange(cumul_abnormal)

bank_cumul_ret_short <- returns_data_parsed %>%
  group_by(ticker) %>%
  # filter(row_number() == n())  %>%
  filter(Date == as.Date("2023-03-10")) %>%
  arrange(cumul_abnormal)


bank_cumul_ret <- returns_data_parsed %>%
  group_by(ticker) %>%
  filter(Date == late_date) %>%
  arrange(cumul_abnormal)

bank_cumul_ret_2022 <- returns_data_parsed_2022 %>%
  group_by(ticker) %>%
  # filter(row_number() == n())  %>%
  filter(row_number() == n()) %>%
  arrange(cumul_abnormal)




feols(cumul_abnormal ~ cumul_abnormal_2022,
  data = bank_cumul_ret_2022 %>%
    select(ticker, cumul_abnormal_2022 = cumul_abnormal) %>%
    mutate(cumul_abnormal_2022 = (1 + cumul_abnormal_2022)^(1 / 365) - 1) %>%
    inner_join(bank_cumul_ret_early,
      by = c("ticker")
    ) %>% mutate(cumul_abnormal = (1 + cumul_abnormal)^(1 / 47) - 1) %>%
    left_join(final_bank_data %>% mutate(assets_imp = as.numeric(assets_imp)) %>%
      select(assets_imp, ticker = TICKER)),
  vcov = "hetero"
)


## Linked FRB-Call-FDIC-Return Sample ----
bank_cumul_ret_linked_y9c <- bank_cumul_ret %>%
  inner_join(final_bank_data, by = c("ticker" = "TICKER")) %>%
  left_join(bank_cumul_ret_2022 %>%
    select(ticker, cumul_abnormal_2022 = cumul_abnormal))
bank_cumul_ret_linked_fdic <- bank_cumul_ret %>%
  inner_join(fdic_deposit_data, by = c("ticker" = "TICKER")) %>%
  mutate(dep_unins_share = (1 - DEPINS / DEP)) %>%
  left_join(bank_cumul_ret_2022 %>%
    select(ticker, cumul_abnormal_2022 = cumul_abnormal))

bank_cumul_ret_linked_y9c_early <- bank_cumul_ret_early %>%
  inner_join(final_bank_data, by = c("ticker" = "TICKER")) %>%
  left_join(bank_cumul_ret_2022 %>%
    select(ticker, cumul_abnormal_2022 = cumul_abnormal))
bank_cumul_ret_linked_y9c_short <- bank_cumul_ret_short %>%
  inner_join(final_bank_data, by = c("ticker" = "TICKER")) %>%
  left_join(bank_cumul_ret_2022 %>%
    select(ticker, cumul_abnormal_2022 = cumul_abnormal))
bank_cumul_ret_linked_fdic_early <- bank_cumul_ret_early %>%
  inner_join(fdic_deposit_data, by = c("ticker" = "TICKER")) %>%
  mutate(dep_unins_share = (1 - DEPINS / DEP))


bank_multiple_cumul_ret_linked_y9c <- bank_cumul_ret_multiple %>%
  inner_join(final_bank_data, by = c("ticker" = "TICKER"))
bank_multiple_cumul_ret_linked_fdic <- bank_cumul_ret_multiple %>%
  inner_join(fdic_deposit_data, by = c("ticker" = "TICKER")) %>%
  mutate(dep_unins_share = (1 - DEPINS / DEP))

bank_returns_data_parsed_y9c <-
  returns_data_parsed %>%
  inner_join(final_bank_data, by = c("ticker" = "TICKER"))



# Counts of Sample for Data Description
dim(bank_sample)
dim(bank_cumul_ret)
dim(fdic_deposit_data)
dim(bank_balance_sheet_sample)
theme_set(theme_minimal(base_size = 16))
bank_list <- c(bank_cumul_ret_early[1:8, ] %>% pull(ticker))

# Figure 1 ----
ggplot(data = sp_data_parsed_2022 %>%
  left_join(sample_bank_return_2022) %>%
  mutate(group = "Feb 2022-Feb 2023") %>% bind_rows(
    sp_data_parsed %>% left_join(sample_bank_return) %>%
      mutate(group = "Feb 2023 - May 2023")
  ) %>% select(Date, cumul_mkt_ret, cumul_bankidx_ret, cumul_ret, group) %>%
  gather(index_name, cumul_ret, -group, -Date) %>%
  mutate(index_name = factor(index_name,
    levels = c(
      "cumul_mkt_ret",
      "cumul_bankidx_ret",
      "cumul_ret"
    ),
    labels = c(
      "S&P 500",
      "DJ Bank Index",
      "Asset-Weighted\nSample Average"
    )
  ))) +
  geom_line(aes(x = Date, y = cumul_ret, color = index_name)) +
  facet_wrap(~group, nrow = 2, scales = "free_x") +
  theme(legend.position = c(.2, .2)) +
  labs(
    y = "Cumulative Return Over Period",
    x = "Date",
    color = ""
  ) +
  scale_color_brewer(palette = "Dark2")
ggsave("output/fig1_aggregate_cumulative_return.pdf", width = 8, height = 5, units = "in")

# Figure 2 ----
# Panel A: 2022
label1 <- paste(
  c(
    "Bank Index Return\n Excess of Market:",
    scales::percent(round(bank_cumul_ret_2022$cumul_abnormal_bankidx[1], 3))
  ),
  collapse = "\n"
)
label2 <- paste(
  c(
    "Equal-Weighted Bank Return\n Excess of Market:",
    scales::percent(round(mean(bank_cumul_ret_2022$cumul_abnormal), 3))
  ),
  collapse = "\n"
)
ggplot(data = bank_cumul_ret_2022) +
  geom_density(aes(x = cumul_abnormal)) +
  geom_vline(xintercept = 0, alpha = .2) +
  geom_vline(
    xintercept = bank_cumul_ret_2022$cumul_abnormal_bankidx[1],
    color = RColorBrewer::brewer.pal(3, name = "Dark2")[1],
    linetype = 2
  ) +
  geom_vline(
    xintercept = mean(bank_cumul_ret_2022$cumul_abnormal),
    color = RColorBrewer::brewer.pal(3, name = "Dark2")[2],
    linetype = 3
  ) +
  scale_x_continuous(labels = scales::percent_format(), limits = c(NA, NA)) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    x = "Cumulative Return in Excess of S&P500\n2022-02-01 to 2023-01-31",
    y = "Bank Density",
    color = "Stock Close Date"
  ) +
  annotate("text",
    x = bank_cumul_ret_2022$cumul_abnormal_bankidx[1],
    y = 2.5,
    label = label1,
    vjust = 1,
    hjust = 1
  ) +
  annotate("text",
    x = mean(bank_cumul_ret_2022$cumul_abnormal),
    color = RColorBrewer::brewer.pal(3, name = "Dark2")[2], ,
    y = 3,
    label = label2,
    vjust = 1,
    hjust = 1
  )
ggsave("output/fig2a_2022_density.pdf", width = 5, height = 5, units = "in")

# Panel B: Early 2023
label1 <- paste(
  c(
    "Bank Index Return\nExcess of Market:",
    scales::percent(round(bank_cumul_ret_early$cumul_abnormal_bankidx[1], 3))
  ),
  collapse = "\n"
)
label2 <- paste(
  c(
    "Equal Weighted Bank Return\nExcess of Market:",
    scales::percent(round(mean(bank_cumul_ret_early$cumul_abnormal), 3))
  ),
  collapse = "\n"
)
ggplot(data = bank_cumul_ret_early) +
  geom_density(aes(x = cumul_abnormal)) +
  geom_vline(xintercept = 0, alpha = .2) +
  geom_vline(
    xintercept = bank_cumul_ret_early$cumul_abnormal_bankidx[1],
    color = RColorBrewer::brewer.pal(3, name = "Dark2")[1],
    linetype = 2
  ) +
  geom_vline(
    xintercept = mean(bank_cumul_ret_early$cumul_abnormal),
    color = RColorBrewer::brewer.pal(3, name = "Dark2")[2],
    linetype = 3
  ) +
  scale_x_continuous(labels = scales::percent_format(), limits = c(NA, NA)) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    x = "Cumulative Return in Excess of S&P500\n2023-02-01 to 2023-03-17",
    y = "Bank Density",
    color = "Stock Close Date"
  ) +
  annotate("text",
    x = bank_cumul_ret_early$cumul_abnormal_bankidx[1],
    y = 3.25,
    label = label1,
    vjust = 1,
    hjust = 1
  ) +
  annotate("text",
    x = mean(bank_cumul_ret_early$cumul_abnormal),
    color = RColorBrewer::brewer.pal(3, name = "Dark2")[2], ,
    y = 5,
    label = label2,
    vjust = 1,
    hjust = 1
  )
ggsave("output/fig2b_early_density.pdf", width = 5, height = 5, units = "in")

# Panel C: Late 2023
label1 <- paste(
  c(
    "Bank Index Return\nExcess of Market:",
    scales::percent(round(bank_cumul_ret$cumul_abnormal_bankidx[1], 3))
  ),
  collapse = "\n"
)
label2 <- paste(
  c(
    "Equal Weighted Bank Return\nExcess of Market:",
    scales::percent(round(mean(bank_cumul_ret$cumul_abnormal), 3))
  ),
  collapse = "\n"
)
ggplot(data = bank_cumul_ret) +
  geom_density(aes(x = cumul_abnormal)) +
  geom_vline(xintercept = 0, alpha = .2) +
  geom_vline(
    xintercept = bank_cumul_ret$cumul_abnormal_bankidx[1],
    color = RColorBrewer::brewer.pal(3, name = "Dark2")[1],
    linetype = 2
  ) +
  geom_vline(
    xintercept = mean(bank_cumul_ret$cumul_abnormal),
    color = RColorBrewer::brewer.pal(3, name = "Dark2")[2],
    linetype = 3
  ) +
  scale_x_continuous(labels = scales::percent_format(), limits = c(NA, NA)) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    x = "Cumulative Return in Excess of S&P500\n2023-02-01 to 2023-05-25",
    y = "Bank Density",
    color = "Stock Close Date"
  ) +
  annotate("text",
    x = bank_cumul_ret$cumul_abnormal_bankidx[1],
    y = 2,
    label = label1,
    vjust = 1,
    hjust = 1
  ) +
  annotate("text",
    x = mean(bank_cumul_ret$cumul_abnormal),
    color = RColorBrewer::brewer.pal(3, name = "Dark2")[2], ,
    y = 3,
    label = label2,
    vjust = 1,
    hjust = 1
  )
ggsave("output/fig2c_late_density.pdf", width = 5, height = 5, units = "in")

# Panel C: Late 2023
label1 <- paste(
  c(
    "Bank Index Return\nExcess of Market:",
    scales::percent(round(bank_cumul_ret[bank_cumul_ret$ticker != "FCNCA", ]$cumul_abnormal_bankidx[1], 3))
  ),
  collapse = "\n"
)
label2 <- paste(
  c(
    "Equal Weighted Bank Return\nExcess of Market\n (Excluding FCNCA):",
    scales::percent(round(mean(bank_cumul_ret[bank_cumul_ret$ticker != "FCNCA", ]$cumul_abnormal), 3))
  ),
  collapse = "\n"
)
ggplot(data = bank_cumul_ret %>% filter(ticker != "FCNCA")) +
  geom_density(aes(x = cumul_abnormal)) +
  geom_vline(xintercept = 0, alpha = .2) +
  geom_vline(
    xintercept = bank_cumul_ret[bank_cumul_ret$ticker != "FCNCA", ]$cumul_abnormal_bankidx[1],
    color = RColorBrewer::brewer.pal(3, name = "Dark2")[1],
    linetype = 2
  ) +
  geom_vline(
    xintercept = mean(bank_cumul_ret[bank_cumul_ret$ticker != "FCNCA", ]$cumul_abnormal),
    color = RColorBrewer::brewer.pal(3, name = "Dark2")[2],
    linetype = 3
  ) +
  scale_x_continuous(labels = scales::percent_format(), limits = c(NA, NA)) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    x = "Cumulative Return in Excess of S&P500\n2023-02-01 to 2023-05-25",
    y = "Bank Density",
    color = "Stock Close Date"
  ) +
  annotate("text",
    x = bank_cumul_ret[bank_cumul_ret$ticker != "FCNCA", ]$cumul_abnormal_bankidx[1],
    y = 2,
    label = label1,
    vjust = 1,
    hjust = 1
  ) +
  annotate("text",
    x = mean(bank_cumul_ret[bank_cumul_ret$ticker != "FCNCA", ]$cumul_abnormal),
    color = RColorBrewer::brewer.pal(3, name = "Dark2")[2], ,
    y = 3,
    label = label2,
    vjust = 1,
    hjust = 1
  )
ggsave("output/fig2c_late_density2.pdf", width = 5, height = 5, units = "in")
# Figure 3 ----

ggplot(data = bank_cumul_ret_2022 %>%
  select(ticker, cumul_abnormal_2022 = cumul_abnormal) %>%
  inner_join(bank_cumul_ret_early,
    by = c("ticker")
  )) +
  geom_point(
    aes(y = cumul_abnormal, x = cumul_abnormal_2022)
  ) +
  geom_smooth(
    aes(y = cumul_abnormal, x = cumul_abnormal_2022),
    method = "lm"
  ) +
  geom_text_repel(aes(y = cumul_abnormal, x = cumul_abnormal_2022, label = ticker)) +
  labs(
    x = "Cumulative Abnormal Returns\n2022-02-01 to 2023-01-31",
    y = "Cumulative Abnormal Returns\n2023-02-01 to 2023-03-17"
  )
ggsave("output/fig3_2022_v_2023_returns.pdf", width = 8, height = 6)

### Trying out bank beta
ggplot(data = bank_cumul_ret_2022 %>%
  select(ticker, cumul_abnormal_2022 = cumul_abnormal) %>%
  inner_join(bank_cumul_ret_early,
    by = c("ticker")
  )) +
  geom_point(
    aes(y = cumul_abnormal, x = beta_bank)
  ) +
  geom_smooth(
    aes(y = cumul_abnormal, x = beta_bank),
    method = "lm"
  ) +
  geom_text_repel(aes(y = cumul_abnormal, x = beta_bank, label = ticker)) +
  labs(
    x = "Factor loading on Bank Index in Excess of S&P500\n2022-02-01 to 2023-01-31",
    y = "Cumulative Abnormal Returns\n2023-02-01 to 2023-03-17"
  )

feols(cumul_abnormal ~ beta_bank + cumul_abnormal_2022,
  data = bank_cumul_ret_2022 %>%
    select(ticker, cumul_abnormal_2022 = cumul_abnormal) %>%
    inner_join(bank_cumul_ret_early,
      by = c("ticker")
    )
)



# Figure 4 ----
ggplot(data = returns_data_parsed %>%
  filter(ticker %in% bank_list & ticker != "SI")) +
  geom_line(
    data = returns_data_parsed %>% filter(!(ticker %in% bank_list)),
    aes(y = cumul_abnormal, x = Date, group = ticker),
    color = "grey",
    alpha = 0.1
  ) +
  geom_line(
    data = sp_data_parsed,
    aes(
      y = cumul_abnormal_bankidx,
      x = Date
    ),
    color = "black",
  ) +
  geom_line(aes(y = cumul_abnormal, x = Date, color = ticker)) +
  # geom_point(aes(y = cumul_abnormal, x = Date, color = ticker)) +
  geom_text_repel(
    data = returns_data_parsed %>%
      filter(ticker %in% bank_list &
        Date == as.Date("2023-05-25")),
    aes(y = cumul_abnormal, x = Date, label = ticker, color = ticker),
    hjust = -1,
    vjust = -1
  ) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(-1.2, .4)) +
  scale_x_date(limits = c(cutoff_date, as.Date("2023-05-30"))) +
  scale_colour_brewer(palette = "Dark2") +
  labs(
    y = "Cumulative Return Excess of Market",
    x = "Date"
  ) +
  theme_minimal(base_size = 18) +
  theme(legend.position = "none") +
  annotate("text",
    x = as.Date("2023-03-30"), y = 0.1,
    label = "Other Bank Returns in Grey"
  ) +
  annotate("text",
    x = as.Date("2023-05-15"), y = -0.11,
    label = "Bank Index\n Excess of S&P500"
  ) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_hline(yintercept = -1, linetype = 2)
ggsave("output/returns_over_time.pdf", width = 8, height = 5)

returns_data_parsed %>%
  filter(Date == as.Date("2023-03-16")) %>%
  ungroup() %>%
  summarize(weighted.mean(cumul_abnormal), n = n())

# Table 1  ----
library(vtable)

rhs_vars <- c(
  "assets_imp", "cash_asset_ratio", "securities_asset_ratio", "liquid_asset_ratio",
  "unrealized_htm_losses_rat_tier1", "htm_ratio", "dep_unins_share", "tier1capratio"
)
labs <- c(
  "Assets (000)",
  "Cash / Assets",
  "Securities / Assets",
  "Liquid Assets / Assets",
  "Unrealized HTM Losses / Tier 1 Capital",
  "Hold-to-Maturity Securities / Assets",
  "Unisured Deposit Share",
  "Tier 1 Capital Ratio"
)
# Several banks are missing Tier 1 Capital Ratio and Tier 1 Capital
# this is for various reporting reasons (e.g. they are small and use a
# community bank leverage ratio (CBLR) framework)

st(
  bank_cumul_ret_linked_y9c_early %>%
    select(all_of(rhs_vars)),
  numformat = c("percent", "assets_imp" = "comma"),
  summ = c("notNA(x)", "mean(x)", "median(x)", "sd(x)"),
  labels = labs,
  out = "latex",
  file = "output/table1_y9c.tex"
)





# Summary Stats ----
returns_data_parsed %>%
  filter(ticker == "SIVB" & Date == as.Date("2023-03-08")) %>%
  ungroup() %>%
  summarize(weighted.mean(cumul_abnormal), n = n())
returns_data_parsed %>%
  filter(ticker != "SIVB" & Date == as.Date("2023-03-08")) %>%
  ungroup() %>%
  summarize(weighted.mean(cumul_abnormal), n = n())
returns_data_parsed %>%
  filter(ticker != "SIVB" & Date == as.Date("2023-03-08")) %>%
  ungroup() %>%
  summarize(quantile = c(0.25, 0.75), quantile(cumul_abnormal, c(0.25, 0.75)), n = n())

sumtable(
  bank_cumul_ret_linked_y9c_early %>%
    select(-cumul_abnormal) %>%
    rename(cumul_abnormal = cumul_abnormal_2022) %>%
    mutate(Date = as.Date("2023-01-31")) %>%
    bind_rows(
      bank_cumul_ret_linked_y9c_early
    ) %>%
    bind_rows(
      bank_cumul_ret_linked_y9c
    ),
  labels = c("Cumulative Abnormal Returns"),
  summ = c("mean(x)", "sd(x)", "pctile(x)[5]", "pctile(x)[25]", "median(x)", "pctile(x)[75]", "pctile(x)[95]"),
  summ.names = c("Mean", "SD", "5th Pct.", "25th Pct.", "50th Pct.", "75th Pct.", "95th Pct."),
  numformat = c("percent"),
  vars = c("cumul_abnormal"), group = "Date", group.long = TRUE,
  out = "latex",
  file = "output/table2.tex",
)

# List of banks to grey out
nonbank <- final_bank_data %>%
  filter(loans_asset_ratio < .2 | deposits_liabilities_ratio < .2) %>%
  pull(TICKER)
# Figure 5 -  Uninsured Deposit Share ----
ggplot(
  data = bank_cumul_ret_linked_y9c_early %>%
    mutate(nonbank = (ticker %in% nonbank)),
  aes(x = dep_unins_share, y = cumul_abnormal)
) +
  geom_point(aes(colour = factor(nonbank))) +
  geom_text_repel(aes(label = ticker, colour = factor(nonbank))) +
  geom_smooth(data = bank_cumul_ret_linked_y9c_early %>%
    filter(!(ticker %in% nonbank))) +
  theme(legend.position = "none", ) +
  labs(
    x = "Uninsured Deposit Share",
    y = "Cumulative Return Excess of Market\n2023-02-01 to 2023-03-17"
  ) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = c("black", "grey"))
ggsave("output/uninsured_deposit_share_v_returns.pdf", width = 8, height = 6)

## HTM Securities and Hidden Losses ----
ggplot(
  data = bank_cumul_ret_linked_y9c_early %>%
    mutate(nonbank = (ticker %in% nonbank)),
  aes(x = htm_ratio, y = cumul_abnormal)
) +
  geom_point() +
  geom_text_repel(aes(label = ticker), size = 8) +
  geom_smooth(data = bank_cumul_ret_linked_y9c_early) +
  theme(legend.position = "none", text = element_text(size = 28)) +
  labs(
    x = "Hold-to-Maturity Asset Share",
    y = "Cumulative Return Excess of Market\n2023-02-01 to 2023-03-17"
  ) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = c("black", "grey"))
ggsave("output/htm_share_v_returns.pdf", width = 8, height = 6)

ggplot(
  data = bank_cumul_ret_linked_y9c_early %>%
    mutate(nonbank = (ticker %in% nonbank)),
  aes(x = unrealized_htm_losses_rat_tier1, y = cumul_abnormal)
) +
  geom_point() +
  geom_text_repel(aes(label = ticker), size = 8) +
  geom_smooth(data = bank_cumul_ret_linked_y9c_early) +
  theme(legend.position = "none", text = element_text(size = 28)) +
  labs(
    x = "Unrealized Hold-to-Maturity Losses / Tier 1 Capital",
    y = "Cumulative Return Excess of Market\n2023-02-01 to 2023-03-17"
  ) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = c("black", "grey"))
ggsave("output/htm_losses_v_returns.pdf", width = 8, height = 6)

## Effects over time of deposits
standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}
bank_returns_data_parsed_y9c <- bank_returns_data_parsed_y9c %>%
  ungroup() %>%
  mutate(
    dep_unins_share_std = standardize(dep_unins_share),
    liquid_asset_ratio_std = standardize(liquid_asset_ratio),
    cash_asset_ratio_std = standardize(cash_asset_ratio),
    securities_asset_ratio_std = standardize(securities_asset_ratio),
    htm_ratio_std = standardize(htm_ratio),
    unrealized_htm_losses_rat_tier1_std = standardize(unrealized_htm_losses_rat_tier1),
    npl_ratio_std = standardize(npl_ratio),
    tier1capratio_std = standardize(tier1capratio)
  )


### Table 2 ----
var_dict <- c(
  liquid_asset_ratio_std = "Liquid Assets / Total Assets",
  dep_unins_share_std = "Uninsured Deposit Share",
  cash_asset_ratio_std = "Cash / Total Assets",
  securities_asset_ratio_std = "Securities / Total Assets",
  htm_ratio_std = "HTM Asset Share",
  unrealized_htm_losses_rat_tier1_std = "Unrealized HTM Losses / Tier 1 Capital",
  cumul_abnormal_2022 = "Cumulative Abnormal Returns (2022)",
  npl_ratio_std = "Non-Performing Loans / Total Loans",
  tier1capratio_std = "Tier 1 Capital Ratio",
  `asset_bin(0,5e+06]` = "Assets [0-5b]",
  `asset_bin(5e+06,1e+07]` = "Assets (5b-10b]",
  `asset_bin(1e+07,5e+07]` = "Assets (10b-50b]",
  `asset_bin(5e+07,2.5e+08]` = "Assets (50b-250b]",
  `asset_bin(2.5e+08,1e+09]` = "Assets (250b-1tr]",
  `asset_bin(1e+09,5e+09]` = "Assets (1tr-10tr]",
  beta_bank = "Beta on Bank Index (Excess of S&P500)"
)

bank_cumul_ret_linked_y9c_early <- bank_cumul_ret_linked_y9c_early %>%
  ungroup() %>%
  mutate(
    dep_unins_share_std = (dep_unins_share - mean(dep_unins_share, na.rm = TRUE)) / sd(dep_unins_share, na.rm = TRUE),
    liquid_asset_ratio_std = (liquid_asset_ratio - mean(liquid_asset_ratio, na.rm = TRUE)) / sd(liquid_asset_ratio, na.rm = TRUE),
    cash_asset_ratio_std = (cash_asset_ratio - mean(cash_asset_ratio, na.rm = TRUE)) / sd(cash_asset_ratio, na.rm = TRUE),
    securities_asset_ratio_std = (securities_asset_ratio - mean(securities_asset_ratio, na.rm = TRUE)) / sd(securities_asset_ratio, na.rm = TRUE),
    htm_ratio_std = (htm_ratio - mean(htm_ratio, na.rm = TRUE)) / sd(htm_ratio, na.rm = TRUE),
    unrealized_htm_losses_rat_tier1_std = (unrealized_htm_losses_rat_tier1 - mean(unrealized_htm_losses_rat_tier1, na.rm = TRUE)
    ) / sd(unrealized_htm_losses_rat_tier1, na.rm = TRUE),
    npl_ratio_std = scale(npl_ratio),
    tier1capratio_std = scale(tier1capratio)
  )
bank_cumul_ret_linked_y9c_short <- bank_cumul_ret_linked_y9c_short %>%
  ungroup() %>%
  mutate(
    dep_unins_share_std = (dep_unins_share - mean(dep_unins_share, na.rm = TRUE)) / sd(dep_unins_share, na.rm = TRUE),
    liquid_asset_ratio_std = (liquid_asset_ratio - mean(liquid_asset_ratio, na.rm = TRUE)) / sd(liquid_asset_ratio, na.rm = TRUE),
    cash_asset_ratio_std = (cash_asset_ratio - mean(cash_asset_ratio, na.rm = TRUE)) / sd(cash_asset_ratio, na.rm = TRUE),
    securities_asset_ratio_std = (securities_asset_ratio - mean(securities_asset_ratio, na.rm = TRUE)) / sd(securities_asset_ratio, na.rm = TRUE),
    htm_ratio_std = (htm_ratio - mean(htm_ratio, na.rm = TRUE)) / sd(htm_ratio, na.rm = TRUE),
    unrealized_htm_losses_rat_tier1_std = (unrealized_htm_losses_rat_tier1 - mean(unrealized_htm_losses_rat_tier1, na.rm = TRUE)
    ) / sd(unrealized_htm_losses_rat_tier1, na.rm = TRUE),
    npl_ratio_std = scale(npl_ratio),
    tier1capratio_std = scale(tier1capratio)
  )
bank_cumul_ret_linked_y9c <- bank_cumul_ret_linked_y9c %>%
  ungroup() %>%
  mutate(
    dep_unins_share_std = (dep_unins_share - mean(dep_unins_share, na.rm = TRUE)) / sd(dep_unins_share, na.rm = TRUE),
    liquid_asset_ratio_std = (liquid_asset_ratio - mean(liquid_asset_ratio, na.rm = TRUE)) / sd(liquid_asset_ratio, na.rm = TRUE),
    cash_asset_ratio_std = (cash_asset_ratio - mean(cash_asset_ratio, na.rm = TRUE)) / sd(cash_asset_ratio, na.rm = TRUE),
    securities_asset_ratio_std = (securities_asset_ratio - mean(securities_asset_ratio, na.rm = TRUE)) / sd(securities_asset_ratio, na.rm = TRUE),
    htm_ratio_std = (htm_ratio - mean(htm_ratio, na.rm = TRUE)) / sd(htm_ratio, na.rm = TRUE),
    unrealized_htm_losses_rat_tier1_std = (unrealized_htm_losses_rat_tier1 - mean(unrealized_htm_losses_rat_tier1, na.rm = TRUE)
    ) / sd(unrealized_htm_losses_rat_tier1, na.rm = TRUE),
    npl_ratio_std = scale(npl_ratio),
    tier1capratio_std = scale(tier1capratio)
  )

est1 <- feols(cumul_abnormal ~ dep_unins_share_std,
  data = bank_cumul_ret_linked_y9c_early, vcov = "HC1"
)
est2 <- feols(cumul_abnormal ~ htm_ratio_std,
  data = bank_cumul_ret_linked_y9c_early, vcov = "HC1"
)
est3 <- feols(cumul_abnormal ~ unrealized_htm_losses_rat_tier1_std,
  data = bank_cumul_ret_linked_y9c_early, vcov = "HC1"
)
est4 <- feols(
  cumul_abnormal ~ dep_unins_share_std +
    htm_ratio_std +
    unrealized_htm_losses_rat_tier1_std,
  data = bank_cumul_ret_linked_y9c_early, vcov = "HC1"
)


est5 <- feols(
  cumul_abnormal ~ dep_unins_share_std +
    htm_ratio_std +
    unrealized_htm_losses_rat_tier1_std +
    htm_ratio_std:dep_unins_share_std +
    unrealized_htm_losses_rat_tier1_std:dep_unins_share_std,
  data = bank_cumul_ret_linked_y9c_early, vcov = "HC1"
)
etable(est1, est2, est3, est4, est5, dict = var_dict)

etable(est1, est2, est3, est4, est5,
  digits = 3, depvar = FALSE, digits.stats = 3,
  style.tex = style.tex("aer"),
  adjustbox = 1.2,
  label = "tab:dep_htm",
  title = "Cumulative returns correlated with uninsured deposits and HTM. This table reports estimated coefficients of regressions with the cumulative returns in excess of the S\\&P 500 index for the banks in our sample from February 1, 2023 to March 17, 2023 as the outcome.  In Column (1), we report the bivariate relationship with the uninsured deposit share (uninsured deposts as a share of total
deposits) measured in 2022q4. Column (2) reports the coefficient with the hold-to-maturity asset share (hold-to-maturity assets
as a share of total assets) measured in 2022q4. Column (3) reports unrealized hold-to-
maturity losses scaled by tier 1 capital. Column (4) combines Columns (1)-(3). Column (5) adds the interaction of uninsured deposit share with HTM Asset Share and Unrealized HTM Losses. All variables (except for the cumulative returns) are mean zero and standarized to have standard deviation one, prior to interactions.",
  dict = var_dict,
  file = "output/table3.tex", replace = TRUE
)

library(ggfixest)
feols(cumul_abnormal_capm ~ i(Date, unrealized_htm_losses_rat_tier1_std) + i(Date, dep_unins_share_std) | Date,
  data = bank_returns_data_parsed_y9c, vcov = "HC1"
) %>%
  broom::tidy() %>%
  # check if term includes either "unrealized_htm_losses_rat_tier1_std" or "dep_unins_share_std"
  filter(str_detect(term, "unrealized_htm_losses_rat_tier1_std|dep_unins_share_std")) %>%
  # seperate on colon
  separate(term, c(NA, NA, "date", "characteristic"), sep = ":") %>%
  # create factor with var_dict
  mutate(characteristic = factor(characteristic,
    levels = names(var_dict),
    labels = var_dict
  )) %>%
  mutate(date = as.Date(date)) %>%
  ggplot(aes(
    x = date,
    y = estimate,
    color = characteristic, shape = characteristic
  )) +
  geom_point() +
  geom_pointrange(
    aes(
      ymin = estimate - 1.96 * std.error,
      ymax = estimate + 1.96 * std.error
    ),
    position = position_dodge2(width = 1), alpha = 0.5
  ) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = as.Date("2023-03-08"), linetype = 2) +
  labs(
    y = "Marginal effect of 1 std. dev. on cumul. returns",
    x = "Date", color = "Characteristic", shape = "Characteristic"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d") +
  # put legend in bottom left
  theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 1))
ggsave("output/table3_over_time.pdf", width = 8, height = 6)




# Table 2, short run ---
est1 <- feols(cumul_abnormal ~ dep_unins_share_std,
  data = bank_cumul_ret_linked_y9c_short, vcov = "HC1"
)
est2 <- feols(cumul_abnormal ~ htm_ratio_std,
  data = bank_cumul_ret_linked_y9c_short, vcov = "HC1"
)
est3 <- feols(cumul_abnormal ~ unrealized_htm_losses_rat_tier1_std,
  data = bank_cumul_ret_linked_y9c_short, vcov = "HC1"
)
est4 <- feols(
  cumul_abnormal ~ dep_unins_share_std +
    htm_ratio_std +
    unrealized_htm_losses_rat_tier1_std,
  data = bank_cumul_ret_linked_y9c_short, vcov = "HC1"
)

est5 <- feols(
  cumul_abnormal ~ dep_unins_share_std +
    htm_ratio_std +
    unrealized_htm_losses_rat_tier1_std +
    htm_ratio_std:dep_unins_share_std +
    unrealized_htm_losses_rat_tier1_std:dep_unins_share_std,
  data = bank_cumul_ret_linked_y9c_short, vcov = "HC1"
)
etable(est1, est2, est3, est4, est5, dict = var_dict)

etable(est1, est2, est3, est4, est5,
  digits = 3, depvar = FALSE, digits.stats = 3,
  style.tex = style.tex("aer"),
  adjustbox = 1.2,
  label = "tab:dep_htm_short",
  title = "Cumulative returns correlated with uninsured deposits and HTM, immediately after SVB shutdown. This table reports estimated coefficients of regressions with the cumulative returns in excess of the S\\&P 500 index for the banks in our sample from February 1, 2023 to March 10, 2023 as the outcome.  In Column (1), we report the bivariate relationship with the uninsured deposit share (uninsured deposts as a share of total
deposits) measured in 2022q4. Column (2) reports the coefficient with the hold-to-maturity asset share (hold-to-maturity assets
as a share of total assets) measured in 2022q4. Column (3) reports unrealized hold-to-
maturity losses scaled by tier 1 capital. Column (4) combines Columns (1)-(3). Column (5) adds the interaction of uninsured deposit share with HTM Asset Share and Unrealized HTM Losses. All variables (except for the cumulative returns) are mean zero and standarized to have standard deviation one, prior to interactions.",
  dict = var_dict,
  file = "output/table3_short.tex", replace = TRUE
)


## Table 2 with beta model ---
est1 <- feols(cumul_abnormal_capm ~ dep_unins_share_std,
  data = bank_cumul_ret_linked_y9c_early, vcov = "HC1"
)
est2 <- feols(cumul_abnormal_capm ~ htm_ratio_std,
  data = bank_cumul_ret_linked_y9c_early, vcov = "HC1"
)
est3 <- feols(cumul_abnormal_capm ~ unrealized_htm_losses_rat_tier1_std,
  data = bank_cumul_ret_linked_y9c_early, vcov = "HC1"
)
est4 <- feols(
  cumul_abnormal_capm ~ dep_unins_share_std +
    htm_ratio_std +
    unrealized_htm_losses_rat_tier1_std,
  data = bank_cumul_ret_linked_y9c_early, vcov = "HC1"
)


est5 <- feols(
  cumul_abnormal_capm ~ dep_unins_share_std +
    htm_ratio_std +
    unrealized_htm_losses_rat_tier1_std +
    htm_ratio_std:dep_unins_share_std +
    unrealized_htm_losses_rat_tier1_std:dep_unins_share_std,
  data = bank_cumul_ret_linked_y9c_early, vcov = "HC1"
)
etable(est1, est2, est3, est4, est5, dict = var_dict)

etable(est1, est2, est3, est4, est5,
  digits = 3, depvar = FALSE, digits.stats = 3,
  style.tex = style.tex("aer"),
  adjustbox = 1.2,
  label = "tab:dep_htm_beta",
  title = "Cumulative returns correlated with uninsured deposits and HTM, adjusted for beta. This table reports estimated coefficients of regressions with the cumulative returns in excess of the S\\&P 500 index, adjusting for beta estimated in 2022, for the banks in our sample from February 1, 2023 to March 17, 2023 as the outcome.  In Column (1), we report the bivariate relationship with the uninsured deposit share (uninsured deposts as a share of total deposits) measured in 2022q4. Column (2) reports the coefficient with the hold-to-maturity asset share (hold-to-maturity assets as a share of total assets) measured in 2022q4. Column (3) reports unrealized hold-to-maturity losses scaled by tier 1 capital. Column (4) combines Columns (1)-(3). Column (5) adds the interaction of uninsured deposit share with HTM Asset Share and Unrealized HTM Losses. All variables (except for the cumulative returns) are mean zero and standarized to have standard deviation one, prior to interactions.",
  dict = var_dict,
  file = "output/table3_beta.tex", replace = TRUE
)


est1 <- feols(cumul_abnormal ~ dep_unins_share_std,
  data = bank_cumul_ret_linked_y9c, vcov = "HC1"
)
est2 <- feols(cumul_abnormal ~ htm_ratio_std,
  data = bank_cumul_ret_linked_y9c, vcov = "HC1"
)
est3 <- feols(cumul_abnormal ~ unrealized_htm_losses_rat_tier1_std,
  data = bank_cumul_ret_linked_y9c, vcov = "HC1"
)
est4 <- feols(
  cumul_abnormal ~ dep_unins_share_std +
    htm_ratio_std +
    unrealized_htm_losses_rat_tier1_std,
  data = bank_cumul_ret_linked_y9c, vcov = "HC1"
)


est5 <- feols(
  cumul_abnormal ~ dep_unins_share_std +
    htm_ratio_std +
    unrealized_htm_losses_rat_tier1_std +
    htm_ratio_std:dep_unins_share_std +
    unrealized_htm_losses_rat_tier1_std:dep_unins_share_std,
  data = bank_cumul_ret_linked_y9c, vcov = "HC1"
)
etable(est1, est2, est3, est4, est5,
  digits = 3,
  depvar = FALSE, digits.stats = 3,
  dict = var_dict
)
etable(est1, est2, est3, est4, est5,
  digits = 3, depvar = FALSE, digits.stats = 3,
  style.tex = style.tex("aer"),
  adjustbox = 1.2,
  label = "tab:dep_htm_late",
  title = "Cumulative returns correlated with uninsured deposits and HTM, long run. This table reports estimated coefficients of regressions with the cumulative returns in excess of the S\\&P 500 index, adjusting for beta estimated in 2022, for the banks in our sample from February 1, 2023 to May 25, 2023 as the outcome.  In Column (1), we report the bivariate relationship with the uninsured deposit share (uninsured deposts as a share of total deposits) measured in 2022q4. Column (2) reports the coefficient with the hold-to-maturity asset share (hold-to-maturity assets as a share of total assets) measured in 2022q4. Column (3) reports unrealized hold-to-maturity losses scaled by tier 1 capital. Column (4) combines Columns (1)-(3). Column (5) adds the interaction of uninsured deposit share with HTM Asset Share and Unrealized HTM Losses. All variables (except for the cumulative returns) are mean zero and standarized to have standard deviation one, prior to interactions.",
  dict = var_dict,
  file = "output/table3_late.tex", replace = TRUE
)
est1 <- feols(cumul_abnormal_capm ~ dep_unins_share_std,
  data = bank_cumul_ret_linked_y9c, vcov = "HC1"
)
est2 <- feols(cumul_abnormal_capm ~ htm_ratio_std,
  data = bank_cumul_ret_linked_y9c, vcov = "HC1"
)
est3 <- feols(cumul_abnormal_capm ~ unrealized_htm_losses_rat_tier1_std,
  data = bank_cumul_ret_linked_y9c, vcov = "HC1"
)
est4 <- feols(
  cumul_abnormal_capm ~ dep_unins_share_std +
    htm_ratio_std +
    unrealized_htm_losses_rat_tier1_std,
  data = bank_cumul_ret_linked_y9c, vcov = "HC1"
)


est5 <- feols(
  cumul_abnormal_capm ~ dep_unins_share_std +
    htm_ratio_std +
    unrealized_htm_losses_rat_tier1_std +
    htm_ratio_std:dep_unins_share_std +
    unrealized_htm_losses_rat_tier1_std:dep_unins_share_std,
  data = bank_cumul_ret_linked_y9c, vcov = "HC1"
)
etable(est1, est2, est3, est4, est5,
  digits = 3,
  depvar = FALSE, digits.stats = 3,
  dict = var_dict
)
etable(est1, est2, est3, est4, est5,
  digits = 3, depvar = FALSE, digits.stats = 3,
  style.tex = style.tex("aer"),
  adjustbox = 1.2,
  label = "tab:dep_htm_late_beta",
  title = "Cumulative returns correlated with uninsured deposits and HTM, long run and adjusted for beta. This table reports estimated coefficients of regressions with the cumulative returns in excess of the S\\&P 500 index for the banks in our sample from February 1, 2023 to May 25, 2023 as the outcome.  In Column (1), we report the bivariate relationship with the uninsured deposit share (uninsured deposts as a share of total deposits) measured in 2022q4. Column (2) reports the coefficient with the hold-to-maturity asset share (hold-to-maturity assets as a share of total assets) measured in 2022q4. Column (3) reports unrealized hold-to-maturity losses scaled by tier 1 capital. Column (4) combines Columns (1)-(3). Column (5) adds the interaction of uninsured deposit share with HTM Asset Share and Unrealized HTM Losses. All variables (except for the cumulative returns) are mean zero and standarized to have standard deviation one, prior to interactions.",
  dict = var_dict,
  file = "output/table3_late_beta.tex", replace = TRUE
)


## Figure 7 Liquid Assets ----
### Liquid Assets ----
ggplot(
  data = bank_cumul_ret_linked_y9c_early %>%
    mutate(nonbank = (ticker %in% nonbank)),
  aes(x = liquid_asset_ratio, y = cumul_abnormal)
) +
  geom_point() +
  geom_text_repel(aes(label = ticker)) +
  geom_smooth() +
  theme(legend.position = "none") +
  labs(
    x = "Liquid Assets / Total Assets",
    y = "Cumulative Return Excess of Market\n2023-02-01 to 2023-03-17"
  ) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format())
ggsave("output/liquid_assets_ratio_v_returns.pdf", width = 8, height = 6)

### Cash  ----
ggplot(
  data = bank_cumul_ret_linked_y9c_early %>%
    mutate(nonbank = (ticker %in% nonbank)),
  aes(x = cash_asset_ratio, y = cumul_abnormal)
) +
  geom_point() +
  geom_text_repel(aes(label = ticker)) +
  geom_smooth(data = bank_cumul_ret_linked_y9c_early) +
  theme(legend.position = "none") +
  labs(
    x = "Cash Assets / Total Assets",
    y = "Cumulative Return Excess of Market\n2023-02-01 to 2023-03-17"
  ) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = c("black", "grey"))
ggsave("output/cash_assets_ratio_v_returns.pdf", width = 8, height = 6)

### Securities  ----
ggplot(
  data = bank_cumul_ret_linked_y9c_early %>%
    mutate(nonbank = (ticker %in% nonbank)),
  aes(x = securities_asset_ratio, y = cumul_abnormal)
) +
  geom_point() +
  geom_text_repel(aes(label = ticker)) +
  geom_smooth(data = bank_cumul_ret_linked_y9c_early) +
  theme(legend.position = "none") +
  labs(
    x = "Securities / Total Assets",
    y = "Cumulative Return Excess of Market\n2023-02-01 to 2023-03-17"
  ) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = c("black", "grey"))
ggsave("output/securities_assets_ratio_v_returns.pdf", width = 8, height = 6)


### Table 3----
est1 <- feols(cumul_abnormal ~ liquid_asset_ratio_std,
  data = bank_cumul_ret_linked_y9c_early, vcov = "HC1"
)
est2 <- feols(cumul_abnormal ~ liquid_asset_ratio_std + dep_unins_share_std,
  data = bank_cumul_ret_linked_y9c_early, vcov = "HC1"
)
est3 <- feols(cumul_abnormal ~ cash_asset_ratio_std + securities_asset_ratio_std,
  data = bank_cumul_ret_linked_y9c_early, vcov = "HC1"
)
est4 <- feols(cumul_abnormal ~ cash_asset_ratio_std + securities_asset_ratio_std + dep_unins_share_std,
  data = bank_cumul_ret_linked_y9c_early, vcov = "HC1"
)
est5 <- feols(cumul_abnormal ~ cash_asset_ratio_std + securities_asset_ratio_std + dep_unins_share_std + cash_asset_ratio_std:dep_unins_share_std + securities_asset_ratio_std:dep_unins_share_std,
  data = bank_cumul_ret_linked_y9c_early, vcov = "HC1"
)
etable(est1, est3, est2, est4, est5)
etable(est1, est3, est2, est4, est5,
  digits = 3, depvar = FALSE, digits.stats = 3,
  style.tex = style.tex("aer"),
  adjustbox = 1.2,
  label = "tab:liquid_assets",
  title = "Cumulative returns correlated with liquid assets. This table reports estimated coefficients of regressions with the cumulative returns in excess of the S\\&P 500 index for the banks in our sample from February 1, 2023 to March 17, 2023 as the outcome. %
   In Column (1), we report the bivariate relationship with the liquid asset share (securities + cash scaled by total
assets) measured in 2022q4. %
   Column (2) adds uninsured deposit share to Column (1). %
   Column (3) reports the coefficient with the cash scaled by total assets and securities scaled by total assets. %
    Column (4) adds uninsured deposit share to Column (3). %
    Column (5) interacts uninsured deposit share with cash and securities. %
    All variables (except for the cumulative returns) are mean zero and standarized to have standard deviation one, prior to interactions.",
  dict = var_dict,
  file = "output/table4.tex", replace = TRUE
)

feols(cumul_abnormal_capm ~ i(Date, cash_asset_ratio_std) + i(Date, securities_asset_ratio_std) + i(Date, dep_unins_share_std) | Date,
  data = bank_returns_data_parsed_y9c, vcov = "HC1"
) %>%
  broom::tidy() %>%
  # check if term includes either "cash_asset_ratio_std" or "securities_asset_ratio_std" or "dep_unins_share_std"
  filter(str_detect(term, "cash_asset_ratio_std|securities_asset_ratio_std")) %>%
  # seperate on colon
  separate(term, c(NA, NA, "date", "characteristic"), sep = ":") %>%
  # create factor with var_dict
  mutate(characteristic = factor(characteristic,
    levels = names(var_dict),
    labels = var_dict
  )) %>%
  mutate(date = as.Date(date)) %>%
  ggplot(aes(
    x = date,
    y = estimate,
    color = characteristic, shape = characteristic
  )) +
  geom_point() +
  geom_pointrange(
    aes(
      ymin = estimate - 1.96 * std.error,
      ymax = estimate + 1.96 * std.error
    ),
    position = position_dodge2(width = 1), alpha = 0.5
  ) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = as.Date("2023-03-08"), linetype = 2) +
  labs(
    y = "Marginal effect of 1 std. dev. on cumul. returns",
    x = "Date", color = "Characteristic", shape = "Characteristic"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d") +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 1))
ggsave("output/table4_over_time.pdf", width = 8, height = 6)

est1 <- feols(cumul_abnormal_capm ~ liquid_asset_ratio_std,
  data = bank_cumul_ret_linked_y9c_early, vcov = "HC1"
)
est2 <- feols(cumul_abnormal_capm ~ liquid_asset_ratio_std + dep_unins_share_std,
  data = bank_cumul_ret_linked_y9c_early, vcov = "HC1"
)
est3 <- feols(cumul_abnormal_capm ~ cash_asset_ratio_std + securities_asset_ratio_std,
  data = bank_cumul_ret_linked_y9c_early, vcov = "HC1"
)
est4 <- feols(cumul_abnormal_capm ~ cash_asset_ratio_std + securities_asset_ratio_std + dep_unins_share_std,
  data = bank_cumul_ret_linked_y9c_early, vcov = "HC1"
)
est5 <- feols(cumul_abnormal_capm ~ cash_asset_ratio_std + securities_asset_ratio_std + dep_unins_share_std + cash_asset_ratio_std:dep_unins_share_std + securities_asset_ratio_std:dep_unins_share_std,
  data = bank_cumul_ret_linked_y9c_early, vcov = "HC1"
)
etable(est1, est3, est2, est4, est5)
etable(est1, est3, est2, est4, est5,
  digits = 3, depvar = FALSE, digits.stats = 3,
  style.tex = style.tex("aer"),
  adjustbox = 1.2,
  label = "tab:liquid_assets_beta",
  title = "Cumulative returns correlated with liquid assets, adjusted for beta. This table reports estimated coefficients of regressions with the cumulative returns in excess of the S\\&P 500 index, adjusted for beta estimated in 2002, for the banks in our sample from February 1, 2023 to March 17, 2023 as the outcome. %
   In Column (1), we report the bivariate relationship with the liquid asset share (securities + cash scaled by total
assets) measured in 2022q4. %
   Column (2) adds uninsured deposit share to Column (1). %
   Column (3) reports the coefficient with the cash scaled by total assets and securities scaled by total assets. %
    Column (4) adds uninsured deposit share to Column (3). %
    Column (5) interacts uninsured deposit share with cash and securities. %
    All variables (except for the cumulative returns) are mean zero and standarized to have standard deviation one, prior to interactions.",
  dict = var_dict,
  file = "output/table4_beta.tex", replace = TRUE
)
est1 <- feols(cumul_abnormal ~ liquid_asset_ratio_std,
  data = bank_cumul_ret_linked_y9c, vcov = "HC1"
)
est2 <- feols(cumul_abnormal ~ liquid_asset_ratio_std + dep_unins_share_std,
  data = bank_cumul_ret_linked_y9c, vcov = "HC1"
)
est3 <- feols(cumul_abnormal ~ cash_asset_ratio_std + securities_asset_ratio_std,
  data = bank_cumul_ret_linked_y9c, vcov = "HC1"
)
est4 <- feols(cumul_abnormal ~ cash_asset_ratio_std + securities_asset_ratio_std + dep_unins_share_std,
  data = bank_cumul_ret_linked_y9c, vcov = "HC1"
)
est5 <- feols(cumul_abnormal ~ cash_asset_ratio_std + securities_asset_ratio_std + dep_unins_share_std + cash_asset_ratio_std:dep_unins_share_std + securities_asset_ratio_std:dep_unins_share_std,
  data = bank_cumul_ret_linked_y9c, vcov = "HC1"
)
etable(est1, est3, est2, est4, est5)
etable(est1, est3, est2, est4, est5,
  digits = 3, depvar = FALSE, digits.stats = 3,
  style.tex = style.tex("aer"),
  adjustbox = 1.2,
  label = "tab:liquid_assets_late",
  title = "Cumulative returns correlated with liquid assets, long run. This table reports estimated coefficients of regressions with the cumulative returns in excess of the S\\&P 500 index for the banks in our sample from February 1, 2023 to March 17, 2023 as the outcome. %
   In Column (1), we report the bivariate relationship with the liquid asset share (securities + cash scaled by total
assets) measured in 2022q4. %
   Column (2) adds uninsured deposit share to Column (1). %
   Column (3) reports the coefficient with the cash scaled by total assets and securities scaled by total assets. %
    Column (4) adds uninsured deposit share to Column (3). %
    Column (5) interacts uninsured deposit share with cash and securities. %
    All variables (except for the cumulative returns) are mean zero and standarized to have standard deviation one, prior to interactions.",
  dict = var_dict,
  file = "output/table4_late.tex", replace = TRUE
)
est1 <- feols(cumul_abnormal_capm ~ liquid_asset_ratio_std,
  data = bank_cumul_ret_linked_y9c, vcov = "HC1"
)
est2 <- feols(cumul_abnormal_capm ~ liquid_asset_ratio_std + dep_unins_share_std,
  data = bank_cumul_ret_linked_y9c, vcov = "HC1"
)
est3 <- feols(cumul_abnormal_capm ~ cash_asset_ratio_std + securities_asset_ratio_std,
  data = bank_cumul_ret_linked_y9c, vcov = "HC1"
)
est4 <- feols(cumul_abnormal_capm ~ cash_asset_ratio_std + securities_asset_ratio_std + dep_unins_share_std,
  data = bank_cumul_ret_linked_y9c, vcov = "HC1"
)
est5 <- feols(cumul_abnormal_capm ~ cash_asset_ratio_std + securities_asset_ratio_std + dep_unins_share_std + cash_asset_ratio_std:dep_unins_share_std + securities_asset_ratio_std:dep_unins_share_std,
  data = bank_cumul_ret_linked_y9c, vcov = "HC1"
)
etable(est1, est3, est2, est4, est5)
etable(est1, est3, est2, est4, est5,
  digits = 3, depvar = FALSE, digits.stats = 3,
  style.tex = style.tex("aer"),
  adjustbox = 1.2,
  label = "tab:liquid_assets_late_beta",
  title = "Cumulative returns correlated with liquid assets, adjusted for beta, long run. This table reports estimated coefficients of regressions with the cumulative returns in excess of the S\\&P 500 index, adjusted for beta estimated in 2002, for the banks in our sample from February 1, 2023 to March 17, 2023 as the outcome. %
   In Column (1), we report the bivariate relationship with the liquid asset share (securities + cash scaled by total
assets) measured in 2022q4. %
   Column (2) adds uninsured deposit share to Column (1). %
   Column (3) reports the coefficient with the cash scaled by total assets and securities scaled by total assets. %
    Column (4) adds uninsured deposit share to Column (3). %
    Column (5) interacts uninsured deposit share with cash and securities. %
    All variables (except for the cumulative returns) are mean zero and standarized to have standard deviation one, prior to interactions.",
  dict = var_dict,
  file = "output/table4_late_beta.tex", replace = TRUE
)


## Capitalization ----
ggplot(
  data = bank_cumul_ret_linked_y9c_early %>%
    mutate(nonbank = (ticker %in% nonbank)),
  aes(x = tier1capratio, y = cumul_abnormal)
) +
  geom_point() +
  geom_text_repel(aes(label = ticker)) +
  geom_smooth(data = bank_cumul_ret_linked_y9c_early) +
  theme(legend.position = "none") +
  labs(
    x = "Tier 1 Capital Ratio",
    y = "Cumulative Return Excess of Market\n2023-02-01 to 2023-03-17"
  ) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = c("black", "grey"))
ggsave("output/tier1_capital_ratio_v_returns.pdf", width = 8, height = 6)

## NPLs ----
ggplot(
  data = bank_cumul_ret_linked_y9c_early %>%
    mutate(nonbank = (ticker %in% nonbank)),
  aes(x = npl_ratio, y = cumul_abnormal)
) +
  geom_point() +
  geom_text_repel(aes(label = ticker)) +
  geom_smooth(data = bank_cumul_ret_linked_y9c_early) +
  theme(legend.position = "none") +
  labs(
    x = "Non-Performing Loan Ratio",
    y = "Cumulative Return Excess of Market\n2023-02-01 to 2023-03-17"
  ) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = c("black", "grey")) +
  geom_hline(yintercept = 0, color = "black", linetype = 2)
ggsave("output/NPL_v_returns.pdf", width = 8, height = 6)


# Table 4 ----
est1 <- feols(cumul_abnormal ~ tier1capratio_std,
  data = bank_cumul_ret_linked_y9c_early, vcov = "HC1"
)
est2 <- feols(cumul_abnormal ~ npl_ratio_std,
  data = bank_cumul_ret_linked_y9c_early, vcov = "HC1"
)
est3 <- feols(cumul_abnormal ~ tier1capratio_std + npl_ratio_std,
  data = bank_cumul_ret_linked_y9c_early, vcov = "HC1"
)
est4 <- feols(
  cumul_abnormal ~
    tier1capratio_std + npl_ratio_std +
    dep_unins_share_std,
  data = bank_cumul_ret_linked_y9c_early, vcov = "HC1"
)
est5 <- feols(
  cumul_abnormal ~
    tier1capratio_std + npl_ratio_std +
    dep_unins_share_std +
    tier1capratio_std:dep_unins_share_std + npl_ratio_std:dep_unins_share_std,
  data = bank_cumul_ret_linked_y9c_early, vcov = "HC1"
)

etable(est1, est2, est3, est4, est5,
  digits = 3, depvar = FALSE, digits.stats = 3,
  style.tex = style.tex("aer"),
  adjustbox = 1.2,
  label = "tab:npl_capital",
  title = "Cumulative returns correlated with NPL and Tier 1 capital. This table reports estimated coefficients of regressions with the cumulative returns in excess of the S\\&P 500 index for the banks in our sample from February 1, 2023 to March 17, 2023 as the outcome. %
   In Column (1), we report the bivariate relationship with the non-performing loan ratio (non-performing loans scaled by total loans) measured in 2022q4. %
   Column (2) reports the coefficient with the the tier 1 capital ratio measured in 2022q4. %
   Column (3) combines Column (1) and (2). %
    Column (4) adds uninsured deposit share to Column (3). %
    Column (5) interacts uninsured deposit share with non-performing loans and tier 1 capital. %
    All variables (except for the cumulative returns) are mean zero and standarized to have standard deviation one, prior to interactions.",
  dict = var_dict,
  file = "output/table5.tex", replace = TRUE
)

feols(cumul_abnormal_capm ~ i(Date, npl_ratio_std) + i(Date, tier1capratio_std) + i(Date, dep_unins_share_std) | Date,
  data = bank_returns_data_parsed_y9c, vcov = "HC1"
) %>%
  broom::tidy() %>%
  # check if term includes either "cash_asset_ratio_std" or "securities_asset_ratio_std" or "dep_unins_share_std"
  filter(str_detect(term, "tier1capratio_std|npl_ratio_std")) %>%
  # seperate on colon
  separate(term, c(NA, NA, "date", "characteristic"), sep = ":") %>%
  # create factor with var_dict
  mutate(characteristic = factor(characteristic,
    levels = names(var_dict),
    labels = var_dict
  )) %>%
  mutate(date = as.Date(date)) %>%
  ggplot(aes(
    x = date,
    y = estimate,
    color = characteristic, shape = characteristic
  )) +
  geom_point() +
  geom_pointrange(
    aes(
      ymin = estimate - 1.96 * std.error,
      ymax = estimate + 1.96 * std.error
    ),
    position = position_dodge2(width = 1), alpha = 0.5
  ) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = as.Date("2023-03-08"), linetype = 2) +
  labs(
    y = "Marginal effect of 1 std. dev. on cumul. returns",
    x = "Date", color = "Characteristic", shape = "Characteristic"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d") +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 1))
ggsave("output/table5_over_time.pdf", width = 8, height = 6)


est1 <- feols(cumul_abnormal_capm ~ tier1capratio_std,
  data = bank_cumul_ret_linked_y9c_early, vcov = "HC1"
)
est2 <- feols(cumul_abnormal_capm ~ npl_ratio_std,
  data = bank_cumul_ret_linked_y9c_early, vcov = "HC1"
)
est3 <- feols(cumul_abnormal_capm ~ tier1capratio_std + npl_ratio_std,
  data = bank_cumul_ret_linked_y9c_early, vcov = "HC1"
)
est4 <- feols(
  cumul_abnormal_capm ~
    tier1capratio_std + npl_ratio_std +
    dep_unins_share_std,
  data = bank_cumul_ret_linked_y9c_early, vcov = "HC1"
)
est5 <- feols(
  cumul_abnormal_capm ~
    tier1capratio_std + npl_ratio_std +
    dep_unins_share_std +
    tier1capratio_std:dep_unins_share_std + npl_ratio_std:dep_unins_share_std,
  data = bank_cumul_ret_linked_y9c_early, vcov = "HC1"
)

etable(est1, est2, est3, est4, est5,
  digits = 3, depvar = FALSE, digits.stats = 3,
  style.tex = style.tex("aer"),
  adjustbox = 1.2,
  label = "tab:npl_capital_beta",
  title = "Cumulative returns correlated with NPL and Tier 1 capital, adjusted for beta. This table reports estimated coefficients of regressions with the cumulative returns in excess of the S\\&P 500 index, adjusted for beta estimated in 2002, for the banks in our sample from February 1, 2023 to March 17, 2023 as the outcome. %
   In Column (1), we report the bivariate relationship with the non-performing loan ratio (non-performing loans scaled by total loans) measured in 2022q4. %
   Column (2) reports the coefficient with the the tier 1 capital ratio measured in 2022q4. %
   Column (3) combines Column (1) and (2). %
    Column (4) adds uninsured deposit share to Column (3). %
    Column (5) interacts uninsured deposit share with non-performing loans and tier 1 capital. %
    All variables (except for the cumulative returns) are mean zero and standarized to have standard deviation one, prior to interactions.",
  dict = var_dict,
  file = "output/table5_beta.tex", replace = TRUE
)


est1 <- feols(cumul_abnormal ~ tier1capratio_std,
  data = bank_cumul_ret_linked_y9c, vcov = "HC1"
)
est2 <- feols(cumul_abnormal ~ npl_ratio_std,
  data = bank_cumul_ret_linked_y9c, vcov = "HC1"
)
est3 <- feols(cumul_abnormal ~ tier1capratio_std + npl_ratio_std,
  data = bank_cumul_ret_linked_y9c, vcov = "HC1"
)
est4 <- feols(
  cumul_abnormal ~
    tier1capratio_std + npl_ratio_std +
    dep_unins_share_std,
  data = bank_cumul_ret_linked_y9c, vcov = "HC1"
)
est5 <- feols(
  cumul_abnormal ~
    tier1capratio_std + npl_ratio_std +
    dep_unins_share_std +
    tier1capratio_std:dep_unins_share_std + npl_ratio_std:dep_unins_share_std,
  data = bank_cumul_ret_linked_y9c, vcov = "HC1"
)

etable(est1, est2, est3, est4, est5,
  digits = 3, depvar = FALSE, digits.stats = 3,
  style.tex = style.tex("aer"),
  adjustbox = 1.2,
  label = "tab:npl_capital_late",
  title = "Cumulative returns correlated with NPL and Tier 1 capital, long run. This table reports estimated coefficients of regressions with the cumulative returns in excess of the S\\&P 500 index for the banks in our sample from February 1, 2023 to May 25, 2023 as the outcome. %
   In Column (1), we report the bivariate relationship with the non-performing loan ratio (non-performing loans scaled by total loans) measured in 2022q4. %
   Column (2) reports the coefficient with the the tier 1 capital ratio measured in 2022q4. %
   Column (3) combines Column (1) and (2). %
    Column (4) adds uninsured deposit share to Column (3). %
    Column (5) interacts uninsured deposit share with non-performing loans and tier 1 capital. %
    All variables (except for the cumulative returns) are mean zero and standarized to have standard deviation one, prior to interactions.",
  dict = var_dict,
  file = "output/table5_late.tex", replace = TRUE
)

est1 <- feols(cumul_abnormal_capm ~ tier1capratio_std,
  data = bank_cumul_ret_linked_y9c, vcov = "HC1"
)
est2 <- feols(cumul_abnormal_capm ~ npl_ratio_std,
  data = bank_cumul_ret_linked_y9c, vcov = "HC1"
)
est3 <- feols(cumul_abnormal_capm ~ tier1capratio_std + npl_ratio_std,
  data = bank_cumul_ret_linked_y9c, vcov = "HC1"
)
est4 <- feols(
  cumul_abnormal_capm ~
    tier1capratio_std + npl_ratio_std +
    dep_unins_share_std,
  data = bank_cumul_ret_linked_y9c, vcov = "HC1"
)
est5 <- feols(
  cumul_abnormal_capm ~
    tier1capratio_std + npl_ratio_std +
    dep_unins_share_std +
    tier1capratio_std:dep_unins_share_std + npl_ratio_std:dep_unins_share_std,
  data = bank_cumul_ret_linked_y9c, vcov = "HC1"
)

etable(est1, est2, est3, est4, est5,
  digits = 3, depvar = FALSE, digits.stats = 3,
  style.tex = style.tex("aer"),
  adjustbox = 1.2,
  label = "tab:npl_capital_late_beta",
  title = "Cumulative returns correlated with NPL and Tier 1 capital, adjusted for beta, long run. This table reports estimated coefficients of regressions with the cumulative returns in excess of the S\\&P 500 index, adjusted for beta estimated in 2002, for the banks in our sample from February 1, 2023 to May 25, 2023 as the outcome. %
   In Column (1), we report the bivariate relationship with the non-performing loan ratio (non-performing loans scaled by total loans) measured in 2022q4. %
   Column (2) reports the coefficient with the the tier 1 capital ratio measured in 2022q4. %
   Column (3) combines Column (1) and (2). %
    Column (4) adds uninsured deposit share to Column (3). %
    Column (5) interacts uninsured deposit share with non-performing loans and tier 1 capital. %
    All variables (except for the cumulative returns) are mean zero and standarized to have standard deviation one, prior to interactions.",
  dict = var_dict,
  file = "output/table5_late_beta.tex", replace = TRUE
)


## Assets  ----
ggplot(
  data = bank_cumul_ret_linked_y9c_early,
  aes(x = assets, y = cumul_abnormal)
) +
  geom_point() +
  geom_text_repel(aes(label = ticker)) +
  geom_smooth() +
  theme(legend.position = "none") +
  labs(
    x = "Assets (000s)",
    y = "Cumulative Return Excess of Market\n2023-02-01 to 2023-03-17"
  ) +
  scale_x_log10(labels = scales::dollar_format()) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(-1, 0.5)) +
  scale_color_manual(values = c("black", "grey")) +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  geom_vline(xintercept = 50000000, color = "black", linetype = 3) +
  geom_vline(xintercept = 250000000, color = "black", linetype = 3)
ggsave("output/assets_v_returns.pdf", width = 8, height = 6)


ggplot(
  data = bank_cumul_ret_linked_y9c,
  aes(x = assets, y = cumul_abnormal)
) +
  geom_point() +
  geom_text_repel(aes(label = ticker)) +
  geom_smooth() +
  theme(legend.position = "none") +
  labs(
    x = "Assets (000s)",
    y = "Cumulative Return Excess of Market\n2023-02-01 to 2023-05-25"
  ) +
  scale_x_log10(labels = scales::dollar_format()) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(-1, 0.5)) +
  scale_color_manual(values = c("black", "grey")) +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  geom_vline(xintercept = 50000000, color = "black", linetype = 3) +
  geom_vline(xintercept = 250000000, color = "black", linetype = 3)
ggsave("output/assets_v_returns_late.pdf", width = 8, height = 6)

reg_data <- bank_cumul_ret_linked_y9c_early %>%
  mutate(asset_bin = cut(as.numeric(assets),
    breaks = c(0, 5e6, 1e7, 5e7, 2.5e8, 1e9, 5e9)
  ))

est1 <- feols(cumul_abnormal ~ -1 + asset_bin, data = reg_data)
est2 <- feols(
  cumul_abnormal ~ asset_bin +
    asset_bin +
    dep_unins_share_std +
    tier1capratio_std +
    cash_asset_ratio_std +
    unrealized_htm_losses_rat_tier1_std +
    beta_bank,
  data = reg_data
)
est3 <- feols(
  cumul_abnormal ~ asset_bin +
    asset_bin +
    cumul_abnormal_2022 +
    dep_unins_share_std +
    tier1capratio_std +
    cash_asset_ratio_std +
    unrealized_htm_losses_rat_tier1_std +
    beta_bank +
    cumul_abnormal_2022,
  data = reg_data
)

reg_data <- bank_cumul_ret_linked_y9c %>%
  mutate(asset_bin = cut(as.numeric(assets),
    breaks = c(0, 5e6, 1e7, 5e7, 2.5e8, 1e9, 5e9)
  ))

est4 <- feols(cumul_abnormal ~ -1 + asset_bin, data = reg_data)
est5 <- feols(
  cumul_abnormal ~ asset_bin +
    asset_bin +
    dep_unins_share_std +
    tier1capratio_std +
    cash_asset_ratio_std +
    unrealized_htm_losses_rat_tier1_std +
    beta_bank,
  data = reg_data
)
est6 <- feols(
  cumul_abnormal ~ asset_bin +
    asset_bin +
    dep_unins_share_std +
    tier1capratio_std +
    cash_asset_ratio_std +
    unrealized_htm_losses_rat_tier1_std +
    beta_bank +
    cumul_abnormal_2022,
  data = reg_data
)

etable(est1, est2, est3, est4, est5, est6, dict = var_dict, vcov = "HC1")
etable(est1, est2, est3, est4, est5, est6,
  vcov = "HC1",
  digits = 3, depvar = FALSE, digits.stats = 3,
  style.tex = style.tex("aer"),
  adjustbox = 1.2,
  label = "tab:dep_assets",
  title = "Cumulative returns correlated with asset size. This table reports estimated coefficients of regressions with the cumulative returns in excess of the S\\&P 500 index for the banks in our sample. %
   In Column (1), we report the relationship with binned indicator variables of total assets measured in 2022q4. The bins exhaustively bin out all observations, and do not include a constant, so each coefficient is the average for each bin. %
   Column (2) includes the binned controls for assets (excluding the bin for banks with total assets less than 5 billion dollars) and a constant, as well as the controls for uninsured deposit share, tier 1 capital ratio, cash/ total assets and unrealized hold-to-maturity losses. We also include a control for the individual banks' estimated factor loading (beta) on the bank index in excess of the S\\&P500 in 2022. This captures any systematic loading on overall bank movements. Column 3 adds the cumulative abnormal return from February 1, 2022 to January 31, 2023 to Column (2). Columns (1)-(3) use cumluative returns from February 1, 2023 to March 17, 2023 as the outcome. Columns (4)-(6) repeat Columns (1)-(3) and use cumluative returns from February 1, 2023 to May 25, 2023 as the outcome.
    All variables (except for the cumulative returns) are mean zero and standarized to have standard deviation one, prior to interactions.",
  dict = var_dict,
  file = "output/table5_size.tex", replace = TRUE
)

reg_data <- bank_cumul_ret_linked_y9c_early %>%
  mutate(asset_bin = cut(as.numeric(assets),
    breaks = c(0, 5e6, 1e7, 5e7, 2.5e8, 1e9, 5e9)
  ))

est1 <- feols(cumul_abnormal_capm ~ -1 + asset_bin, data = reg_data)
est2 <- feols(
  cumul_abnormal_capm ~ asset_bin +
    asset_bin +
    dep_unins_share_std +
    tier1capratio_std +
    cash_asset_ratio_std +
    unrealized_htm_losses_rat_tier1_std +
    beta_bank,
  data = reg_data
)
est3 <- feols(
  cumul_abnormal_capm ~ asset_bin +
    asset_bin +
    cumul_abnormal_2022 +
    dep_unins_share_std +
    tier1capratio_std +
    cash_asset_ratio_std +
    unrealized_htm_losses_rat_tier1_std +
    beta_bank +
    cumul_abnormal_2022,
  data = reg_data
)

reg_data <- bank_cumul_ret_linked_y9c %>%
  mutate(asset_bin = cut(as.numeric(assets),
    breaks = c(0, 5e6, 1e7, 5e7, 2.5e8, 1e9, 5e9)
  ))

est4 <- feols(cumul_abnormal_capm ~ -1 + asset_bin, data = reg_data)
est5 <- feols(
  cumul_abnormal_capm ~ asset_bin +
    asset_bin +
    dep_unins_share_std +
    tier1capratio_std +
    cash_asset_ratio_std +
    unrealized_htm_losses_rat_tier1_std +
    beta_bank,
  data = reg_data
)
est6 <- feols(
  cumul_abnormal_capm ~ asset_bin +
    asset_bin +
    dep_unins_share_std +
    tier1capratio_std +
    cash_asset_ratio_std +
    unrealized_htm_losses_rat_tier1_std +
    beta_bank +
    cumul_abnormal_2022,
  data = reg_data
)

etable(est1, est2, est3, est4, est5, est6, dict = var_dict, vcov = "HC1")
etable(est1, est2, est3, est4, est5, est6,
  vcov = "HC1",
  digits = 3, depvar = FALSE, digits.stats = 3,
  style.tex = style.tex("aer"),
  adjustbox = 1.2,
  label = "tab:dep_assets_beta",
  title = "Cumulative returns correlated with asset size, adjusted for beta. This table reports estimated coefficients of regressions with the cumulative returns in excess of the S\\&P 500 index, adjusted for beta estimated in 2002, for the banks in our sample. %
   In Column (1), we report the relationship with binned indicator variables of total assets measured in 2022q4. The bins exhaustively bin out all observations, and do not include a constant, so each coefficient is the average for each bin. %
   Column (2) includes the binned controls for assets (excluding the bin for banks with total assets less than 5 billion dollars) and a constant, as well as the controls for uninsured deposit share, tier 1 capital ratio, cash/ total assets and unrealized hold-to-maturity losses. Column 3 adds the cumulative abnormal return from February 1, 2022 to January 31, 2023 to Column (2). Columns (1)-(3) use cumluative returns from February 1, 2023 to March 17, 2023 as the outcome. Columns (4)-(6) repeat Columns (1)-(3) and use cumluative returns from February 1, 2023 to May 25, 2023 as the outcome.
    All variables (except for the cumulative returns) are mean zero and standarized to have standard deviation one, prior to interactions.",
  dict = var_dict,
  file = "output/table5_size_beta.tex", replace = TRUE
)


reg_data <- bank_cumul_ret_linked_y9c_early %>%
  mutate(asset_bin = cut(as.numeric(assets),
    breaks = c(0, 5e6, 1e7, 5e7, 2.5e8, 1e9, 5e9)
  ))

est1 <- feols(cumul_abnormal_2022 ~ -1 + asset_bin, data = reg_data)
est2 <- feols(
  cumul_abnormal_2022 ~ asset_bin +
    dep_unins_share_std +
    htm_ratio_std +
    unrealized_htm_losses_rat_tier1_std,
  data = reg_data
)
est3 <- feols(
  cumul_abnormal_2022 ~ asset_bin +
    dep_unins_share_std +
    htm_ratio_std +
    unrealized_htm_losses_rat_tier1_std +
    cash_asset_ratio_std +
    securities_asset_ratio_std,
  data = reg_data
)
est4 <- feols(
  cumul_abnormal_2022 ~ asset_bin +
    dep_unins_share_std +
    htm_ratio_std +
    unrealized_htm_losses_rat_tier1_std +
    cash_asset_ratio_std +
    securities_asset_ratio_std +
    tier1capratio_std +
    npl_ratio_std,
  data = reg_data
)

etable(est1, est2, est3, est4, dict = var_dict, vcov = "HC1")
etable(est1, est2, est3, est4,
  vcov = "HC1",
  digits = 3, depvar = FALSE, digits.stats = 3,
  style.tex = style.tex("aer"),
  adjustbox = 1,
  label = "tab:dep_2022",
  title = "Cumulative returns in 2022 correlations. This table reports estimated coefficients of regressions with the cumulative returns in excess of the S\\&P 500 index for the banks in our sample from February 1, 2022 to January 31, 2023. %
   In Column (1), we report the relationship with binned indicator variables of total assets measured in 2022q4. The bins exhaustively bin out all observations, and do not include a constant, so each coefficient is the average for each bin. %
   Column (2) includes the binned controls for assets (excluding the bin for banks with total assets less than 5 billion dollars) and a constant, as well as the controls for uninsured deposit share, hold-to-maturity asset share, and unrealized hold-to-maturity losses.
   Column (3) includes  cash/ total assets and securities / total assets.
   Column (4) includes  tier 1 capital ratio and non-performing loan ratio.
    All variables are mean zero and standarized to have standard deviation one, prior to interactions.",
  dict = var_dict,
  file = "output/table6_2022.tex", replace = TRUE
)

feols(cumul_abnormal_capm ~ i(Date, asset_bin, ref2 = "(0,5e+06]") + i(Date, dep_unins_share_std) + i(Date, tier1capratio_std) + i(Date, cash_asset_ratio_std) + i(Date, unrealized_htm_losses_rat_tier1_std) | Date,
  data = bank_returns_data_parsed_y9c %>%
    mutate(asset_bin = cut(as.numeric(assets),
      breaks = c(0, 5e6, 1e7, 5e7, 2.5e8, 1e9, 5e9)
    )), vcov = "HC1"
) %>%
  broom::tidy() %>%
  # check if term includes either "cash_asset_ratio_std" or "securities_asset_ratio_std" or "dep_unins_share_std"
  filter(str_detect(term, "asset_bin")) %>%
  # seperate on colon
  separate(term, c(NA, NA, "date", "characteristic", NA, "size"), sep = ":") %>%
  # remove 0,5e+06]\" from size
  mutate(size = str_replace(size, "0,5e\\+06]\\\"\\)", "")) %>%
  # create factor with var_dict
  mutate(size = factor(size,
    levels = c("(0,5e+06]", "(5e+06,1e+07]", "(1e+07,5e+07]", "(5e+07,2.5e+08]", "(2.5e+08,1e+09]", "(1e+09,5e+09]"),
    labels = c("0-5b", "5b-10b", "10b-50b", "50b-250b", "250b-1tr", "1tr-10tr")
  )) %>%
  mutate(date = as.Date(date)) %>%
  ggplot(aes(
    x = date,
    y = estimate,
    color = size,
    shape = size
  )) +
  geom_point() +
  geom_pointrange(
    aes(
      ymin = estimate - 1.96 * std.error,
      ymax = estimate + 1.96 * std.error
    ),
    position = position_dodge2(width = 1), alpha = 0.5
  ) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = as.Date("2023-03-08"), linetype = 2) +
  labs(
    y = "Change in cumul returns by asset size relative to (0,5b)",
    x = "Date", color = "Asset Size", shape = "Asset Size"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d") +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 1))
ggsave("output/table6_size_over_time.pdf", width = 8, height = 6)


feols(
  cumul_abnormal_capm ~ i(Date, asset_bin, ref2 = "(0,5e+06]") +
    i(Date, dep_unins_share_std) + i(Date, tier1capratio_std) +
    i(Date, cash_asset_ratio_std) +
    i(Date, unrealized_htm_losses_rat_tier1_std) + i(Date, cumul_abnormal_2022) | Date,
  data = bank_returns_data_parsed_y9c %>%
    mutate(asset_bin = cut(as.numeric(assets),
      breaks = c(0, 5e6, 1e7, 5e7, 2.5e8, 1e9, 5e9)
    )) %>%
    left_join(bank_cumul_ret_2022 %>%
      select(ticker,
        cumul_abnormal_2022 = cumul_abnormal
      )) %>% mutate(cumul_abnormal_2022 = standardize(cumul_abnormal_2022)), vcov = "HC1"
) %>%
  broom::tidy() %>%
  # check if term includes
  filter(str_detect(term, "cumul_abnormal_2022|dep_unins_share_std|tier1capratio_std|cash_asset_ratio_std|unrealized_htm_losses_rat_tier1_std")) %>%
  # seperate on colon
  separate(term, c(NA, NA, "date", "characteristic"), sep = ":") %>%
  # create factor with var_dict
  mutate(characteristic = factor(characteristic,
    levels = names(var_dict),
    labels = var_dict
  )) %>%
  mutate(date = as.Date(date)) %>%
  ggplot(aes(
    x = date,
    y = estimate,
    color = characteristic,
    shape = characteristic
  )) +
  geom_point() +
  geom_pointrange(
    aes(
      ymin = estimate - 1.96 * std.error,
      ymax = estimate + 1.96 * std.error
    ),
    position = position_dodge2(width = 1), alpha = 0.5
  ) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = as.Date("2023-03-08"), linetype = 2) +
  labs(
    y = "Marginal effect of 1 std. dev. on cumul. returns",
    x = "Date", color = "Characteristic", shape = "Characteristic"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d") +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(ncol = 1))
ggsave("output/table6_all_char_over_time.pdf", width = 8, height = 6)


## CAPM (Single Index Model) Excess Returns around date ---
ggplot(
  data = returns_data_parsed_feb15 %>%
    filter(Date < early_date),
  aes(
    y = cumul_abnormal_capm,
    x = Date, group = ticker
  )
) +
  geom_line(color = "grey", alpha = 0.2) +
  geom_point(color = "grey", alpha = 0.2) +
  geom_line(data = returns_data_parsed_feb15 %>%
    filter(Date < early_date) %>% filter(ticker %in% bank_list), aes(color = ticker)) +
  geom_point(data = returns_data_parsed_feb15 %>%
    filter(Date < early_date) %>% filter(ticker %in% bank_list), aes(color = ticker)) +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(
    data = seq.Date(as.Date("2023-02-01"), early_date, by = "days") %>%
      as_tibble() %>%
      rename(Date = value) %>%
      filter(lubridate::wday(Date) %in% c(1, 7)),
    aes(xintercept = Date), color = "grey80", size = 2
  ) +
  scale_color_brewer(palette = "Dark2") +
  # turn legend off
  theme(legend.position = "none") +
  geom_vline(aes(xintercept = as.Date("2023-03-09"))) +
  labs(x = "Date", y = "Cumulative Abnormal Returns From February 15, 2023", color = "Bank") +
  geom_text(
    data = returns_data_parsed_feb15 %>%
      filter(Date == as.Date("2023-03-16")) %>% filter(ticker %in% bank_list),
    aes(
      y = cumul_abnormal_capm,
      x = Date + 1,
      label = ticker,
      color = ticker
    )
  )
