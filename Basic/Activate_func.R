library(ggplot2)
qplot(b)

##함수 파라미터 지정하기
qplot(data=mpg, x=cty)
qplot(data=mpg, y=hwy, x=drv, geom='point')
qplot(data=mpg, y=hwy, x=drv, geom='boxplot')
qplot(data=mpg, y=hwy, x=drv, geom='boxplot',colour=drv)

##파라미터 보는 법
?qplot

qplot(1:10, rnorm(10), colour = runif(10))
qplot(1:10, letters[1:10])
mod <- lm(mpg ~ wt, data = mtcars)
qplot(resid(mod), fitted(mod))

qplot(mpg, wt, data = mtcars)
qplot(mpg, wt, data = mtcars, colour = cyl)
qplot(mpg, wt, data = mtcars, size = cyl)
qplot(mpg, wt, data = mtcars, facets = vs ~ am)

##구글에서 치트시트 검색해서 찾아보기!!