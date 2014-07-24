library(ggplot2)
library(car)

data(mtcars)
?mtcars

str(mtcars)
cor(mtcars)

# names <- rownames(mtcars)
# coding transmission factor variable
mtcars$am <- gsub(0, "automatic", mtcars$am)
mtcars$am <- gsub(1, "manual", mtcars$am)
mtcars$am <- as.factor(mtcars$am)

# new gallons per mile variable
mtcars$gpm <- 100/mtcars$mpg

# mpg scatter with name labels
t <- ggplot(data = mtcars, aes(x = wt, y = mpg, colour = am))
t + geom_point(size = 2.5) +
        geom_text(aes(label = rownames(mtcars)), hjust = 0, vjust = 1, size = 3, family = "mono") +
        coord_cartesian(xlim = c(1, 6.5), ylim = c(5, 40)) +
        xlab("Weight (Lbs/1,000)") + ylab("Miles per Gallon") +
        labs(colour = "Transmission") +
#         ggtitle("Motor Trend Car Road Tests Data Set") + labs(colour = "Transmission") +
        theme_bw() +
        theme(text = element_text(family = "mono"))

# gpm scatter with name labels
p <- ggplot(data = mtcars, aes(x = wt, y = gpm, colour = am))
p + geom_point(size = 2.5) +
        geom_text(aes(label = rownames(mtcars)), hjust = 0, vjust = 1, size = 3, family = "mono") +
        coord_cartesian(xlim = c(1, 6.5), ylim = c(2, 10)) +
        xlab("Weight (Lbs/1,000)") + ylab("Gallons per 100 Miles") +
#        ggtitle("Motor Trend Car Road Tests Data Set") + 
        labs(colour = "Transmission") +
        theme_bw() +
        theme(text = element_text(family = "mono"))

# this shows nothing
# coplot(mpg ~ disp | as.factor(cyl), data = mtcars, panel = panel.smooth, rows = 1)

# hp per weight measure for how over-powered a car is
mtcars$hpwt <- mtcars$hp/mtcars$wt
cor(mtcars$hpwt, mtcars$wt)

# model fit1
fit1 <- lm(gpm ~ hpwt + wt, mtcars)
summary(fit1)
vif(fit1)

# diagnostic plots, which = 1:4
plot(fit1, which = 2)

# diagnostic panel
op <- par(mfrow=c(2,2), cex=.9, omi=c(.1, .1, .1, .1))
plot(fit1, ask=F)
par(op)

# include a/m transmission variable
fit2 <- lm(gpm ~ hpwt + wt + am, mtcars)
summary(fit2)
vif(fit2)

names(mtcars)
mtcars$hpdisp <- mtcars$hp / mtcars$disp

cor(mtcars$hpdisp, mtcars$hpwt)
cor(mtcars$hpdisp, mtcars$wt)