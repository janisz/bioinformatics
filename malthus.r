malthus <- function(initial_time, initial_population, time_interval, N, growth_rate) {
    time <- initial_time
    population <- initial_population
    times <- c(time)
    populations <- c(population)
    for(i in 1:N) {
        population <- population + growth_rate*time_interval*population
        time <- time + time_interval
        times <- c(times, time)
        populations <- c(populations, population)
    }

    return(data.frame(times, populations))
}

plot(malthus(1900, 76.2, 1, 300, 0.013))
