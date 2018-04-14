args <- commandArgs(trailingOnly = TRUE)

mu <- as.numeric(args[1])
sd <- as.numeric(args[2])

b <- rnorm(100, mu, sd)

png(filename = paste0("C:/Users/huber/Desktop/", "wykres1.png"))
hist(b, main = paste("Rozklad normalny o sredniej", mu, "i odchyleniu standardowym", sd))
dev.off()

summary(b)
