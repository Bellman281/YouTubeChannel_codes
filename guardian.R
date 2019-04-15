library(GuardianR)

results <- get_guardian(keywords="spycops",
                        from.date="2018-06-01",
                        to.date="2018-06-08",
                        api.key="a538ddb1-f598-4ffa-abce-bd2e5fc5583d")

results$headline
