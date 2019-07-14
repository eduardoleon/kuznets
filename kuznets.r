library(dplyr)
library(tidyr)

collar = read.csv("collar.csv")

salary = read.csv("salary.csv") %>%
    filter(sex == "SEX_T") %>%
    mutate(salary = obs_value) %>%
    select(time, source, classif1, classif2, salary)

sector = read.csv("sector.csv") %>%
    filter(sex == "SEX_T") %>%
    filter(source == "BX:375") %>%
    mutate(people = obs_value) %>%
    select(time, source, classif1, people)

cpi = read.csv("cpi.csv") %>%
    group_by(year) %>%
    summarise(cpi = mean(cpi)) %>%
    mutate(time = year) %>%
    select(time, cpi)

survey = function(code) {
    temp = salary %>%
        filter(source == code) %>%
        left_join(sector) %>%
        left_join(collar) %>%
        mutate(people = ifelse(is.na(people), 1, people)) %>%
        group_by(time, source, collar, classif2) %>%
        summarise(salary = weighted.mean(salary, people))
    
    data = list()
    
    data$local = temp %>%
        filter(classif2 == "CUR_TYPE_LCU") %>%
        inner_join(cpi) %>%
        mutate(nominal = salary, real = salary * 100 / cpi) %>%
        select(time, source, collar, real) %>%
        spread(key = collar, value = real)
    
    data$ppp = temp %>%
        filter(classif2 == "CUR_TYPE_PPP2011") %>%
        select(time, source, collar, salary) %>%
        spread(key = collar, value = salary)
    
    return (data)
}

bx = survey("BX:375")
da = survey("DA:3092")
fx = survey("FX:379")
ja = survey("JA:377")

write.csv(bx$local, "dump/bx.local.csv")
write.csv(da$local, "dump/da.local.csv")
write.csv(fx$local, "dump/fx.local.csv")
write.csv(ja$local, "dump/ja.local.csv")

write.csv(bx$ppp, "dump/bx.ppp.csv")
write.csv(da$ppp, "dump/da.ppp.csv")
write.csv(fx$ppp, "dump/fx.ppp.csv")
write.csv(ja$ppp, "dump/ja.ppp.csv")
