# transforma.r

library(sqldf)

anos <- c(2003:2016)

distTimao <- c()
distVerdao <- c()

for (ano in anos) {

	# normalizacao de parametros
	df <- read.csv(paste('dados/tratados/', ano, '.csv', sep=''))

	q <- "SELECT MAX(pontos) AS pmax, MAX(v) AS vmax, MAX(e) AS emax, MAX(d) AS dmax,
		MAX(gm) AS gmmax, MAX(gs) AS gsmax FROM df"

	maximos <- sqldf(q)

	df$pontos <- df$pontos / maximos$pmax
	df$v <- df$v / maximos$vmax
	df$e <- df$e / maximos$emax
	df$d <- df$d / maximos$dmax
	df$gm <- df$gm / maximos$gmmax
	df$gs <- df$gs / maximos$gsmax

	# selecao de campeao do ano
	campeao <- df[df$classif == 1, ]

	# calculo da distancia do timao ao campeao
	timao <- NULL
	timao <- df[df$time == 'Corinthians', ]

	# calculo da distancia do verdao ao campeao
	verdao <- NULL
	verdao <- df[df$time == 'Palmeiras', ]

	pnum <- c('pontos', 'v', 'e', 'd', 'gm', 'gs')

	# calcular distancias
	if (nrow(timao) != 0 & nrow(verdao) != 0) {
		distTimao <- append(distTimao, sum((campeao[pnum] - timao[pnum])^2)^(1/2))
		distVerdao <- append(distVerdao, sum((campeao[pnum] - verdao[pnum])^2)^(1/2))
	}

}

# media e desvio de distancias
utimao <- mean(distTimao)
stimao <- sd(distTimao)
mtimao <- median(distTimao)

uverdao <- mean(distVerdao)
sverdao <- sd(distVerdao)
mverdao <- median(distVerdao)

# resultados
print('timao:')
print(utimao)
print(stimao)
print(mtimao)

print('verdao:')
print(uverdao)
print(sverdao)
print(mverdao)

# teste de hipotese
ttest <- t.test(distVerdao, distTimao, alternative='less', conf.level=0.95)
print(ttest)
