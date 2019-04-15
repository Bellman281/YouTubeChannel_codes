library(GuardianR)

results <- get_guardian(keywords="spycops",
                        from.date="2018-06-01",
                        to.date="2018-06-08",
                        api.key="?????????????")

results$headline
