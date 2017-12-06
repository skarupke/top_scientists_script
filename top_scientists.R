library(ggplot2)
library(animation)
library(ggrepel)
library(readstata13)

base_folder <- 'C:\\Users\\Malte\\Desktop\\Star Scientists\\'
dir.create(file.path(base_folder, 'images'), showWarnings = FALSE)

migration_rates <- read.dta13(file.path(base_folder, 'data', 'star_migration_rates.dta'))
migration_rates$total_scientists <- migration_rates['pop_star90_stayers'] + migration_rates['pop_star95_stayers'] + migration_rates['pop_star99_stayers']
migration_rates$scientists_per_million <- 1000000 * migration_rates$total_scientists / migration_rates$pop
migration_rates$income_and_sales_tax_diff <- migration_rates$ASTR_p99_diff + 0.01 * migration_rates$SalesTaxRate_diff
migration_rates$total_outflow <- migration_rates$outflow90 + migration_rates$outflow95 + migration_rates$outflow99
state_data <- subset(migration_rates, state == F_state)

start_year <- min(state_data$year)
end_year <- max(state_data$year)
end_picture_year <- 2003
state_data <- subset(state_data, year <= end_picture_year)

min99_by_year <- c()
for (i in start_year : end_year)
{
  data <- subset(state_data, year == i)
  min99_by_year[i] <- min(data$ASTR_p99)
}

state_taxes_local99 <- state_data$ASTR_p99
years <- state_data$year
for (i in 1:nrow(state_data))
{
  state_taxes_local99[i] <- state_taxes_local99[i] - min99_by_year[years[i]]
}
state_data$state_taxes99 <- state_taxes_local99


all_states <- unique(state_data$state)
sum_migrations <- c()
sum_state_growth <- c()
num_sum_migrations <- 1
for (s in all_states)
{
  move_from_state <- subset(subset(migration_data, F_state != s), state == s)
  move_to_state <- subset(subset(migration_data, state != s), F_state == s)
  stay_in_state <- subset(state_data, state == s)
  original_state_value <- subset(stay_in_state, year == start_year)$total_scientists[1,]
  for (y in start_year:end_picture_year)
  {
    sum_migrations[num_sum_migrations] <- sum(subset(move_to_state, year <= y)$total_outflow) - sum(subset(move_from_state, year <= y)$total_outflow)
    sum_state_growth[num_sum_migrations] <- subset(stay_in_state, year == y)$total_scientists[1,] - original_state_value
    num_sum_migrations <- num_sum_migrations + 1
  }
}

state_data$sum_migration <- sum_migrations
state_data$sum_new_scientists <- sum_state_growth
state_data$sum_homegrown <- sum_state_growth - sum_migrations
state_data$migration_per_million <- 1000000 * state_data$sum_migration / state_data$pop
state_data$homegrown_per_million <- 1000000 * state_data$sum_homegrown / state_data$pop
state_data$percent_migration <- pmax(sum_migrations, 0) / pmax(sum_state_growth, 0)
state_data$percent_migration[is.infinite(state_data$percent_migration)] <- 0
state_data$percent_migration[is.na(state_data$percent_migration)] <- 0
state_data$percent_migration <- pmax(state_data$percent_migration, -1)
state_data$percent_migration <- pmin(state_data$percent_migration, 1)

# need to exclude DC because it doesn't have all the data
gapminder_subset = subset(state_data, state != 'DC')

to_gapminder_format <- function(data) {
    as_gapminder <- data.frame(state=data$state,
				year=data$year,
				state_taxes=data$state_taxes99,
				scientists_per_million=data$scientists_per_million,
				total_scientists=data$total_scientists,
				population=data$pop,
				sales_tax=data$SalesTaxRate * 0.01,
				sales_plus_state_tax=data$SalesTaxRate * 0.01 + data$state_taxes99,
				gsp=data$gsp_nominal,
				gsp_per_person=data$gsp_nominal * 1000000000 / data$pop,
				total_income_tax=data$ATR_p99,
                        sum_migration=data$sum_migration,
                        sum_new_scientists = data$sum_new_scientists,
                        sum_homegrown=data$sum_homegrown,
				migration_per_million=data$migration_per_million,
				homegrown_per_million=data$homegrown_per_million,
                        percent_from_migration = data$percent_migration)
    names(as_gapminder) <- c('state',
				'year',
				'state_income_tax',
				'scientists_per_million',
				'total_scientists',
				'population',
				'sales_tax',
				'sales_plus_income_tax',
				'GSP',
				'GSP_per_person',
				'total_income_tax',
                        'sum_migration',
                        'sum_new_scientists',
                        'sum_homegrown',
				'migration_per_million',
				'homegrown_per_million',
                        'percent_from_migration')
    return(as_gapminder)
}

write.csv(to_gapminder_format(gapminder_subset), file.path(base_folder, 'data', 'for_gapminder.csv'))

plot_one <- function(data, x, y, max_y, filename)
{
  plot <- ggplot(data) + 
	expand_limits(y=c(0, max_y)) +
      geom_point(aes(x=data[x], y=data[y], color=state)) +
	geom_text_repel(aes(x=data[x], y=data[y], color=state, label=state), size = 2) +
	xlab(x) +
	ylab(y)
  ggsave(filename)
}

max_x = max(state_data$state_taxes99)
max_y = max(state_data$scientists_per_million)


for (i in start_year : end_picture_year)
{
  path <- file.path(base_folder, 'images', paste(i, '.png', sep=''))
  data <- subset(state_data, year == i)
  min_x <- min(data$state_taxes99)
  mean_x <- mean(data$state_taxes99)
  plot <- ggplot(data) + 
	#xlim(min_x, min_x + 2.0 * (mean_x - min_x)) +
	#expand_limits(x=c(0, max_x)) +
	expand_limits(y=c(0, max_y)) +
      geom_point(aes(x=state_taxes99, y=scientists_per_million, color=state, size=pop)) +
      #coord_cartesian(xlim = c(min_x, min_x + 2.0 * (mean_x - min_x)), ylim=c(0, max_y)) +
	#scale_y_continunous(limits = c(0, max_y)) +
	geom_text_repel(aes(x=state_taxes99, y=scientists_per_million, color=state, label=state), size = 2)
  #if (i > 1977)
  #{
  #  datas = array(start_year : i)
  #  for(j in start_year : (i - 1)){
  #      data <- subset(state_data, year == j)
  #      plot <- plot + geom_point(data=data, aes(x=state_taxes99, y=scientists_per_million, color=state, size=pop, alpha=0.1), inherit.aes=FALSE)
  #  }
  #}
  ggsave(path)
}

data_1977 <- to_gapminder_format(subset(state_data, year == 1977))
data_2003 <- to_gapminder_format(subset(state_data, year == 2003))
write.csv(data_1977, file.path(base_folder, 'data', 'data_1977.csv'))
write.csv(data_2003, file.path(base_folder, 'data', 'data_2003.csv'))
plot_one(data_2003, 'population', 'total_scientists', max(data_2003['total_scientists']), file.path(base_folder, 'images', 'scientists_by_population.png'))
plot_one(data_1977, 'sales_plus_income_tax', 'scientists_per_million', max(data_1977['scientists_per_million']), file.path(base_folder, 'images', 'distribution_1977.png'))
plot_one(data_1977, 'sales_plus_income_tax', 'total_scientists', max(data_1977['total_scientists']), file.path(base_folder, 'images', 'distribution_total_1977.png'))

migration_data <- subset(migration_rates, state != F_state)
migration_data <- subset(migration_data, total_outflow != 0)
ggplot(migration_data) +
	geom_point(aes(x=income_and_sales_tax_diff, y=total_outflow)) +
	geom_point(aes(x=c(mean(migration_data$income_and_sales_tax_diff * migration_data$total_outflow)), y=c(mean(migration_data$total_outflow)), color='mean'))
ggsave(file.path(base_folder, 'images', 'all_migrations.png'))



