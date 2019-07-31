all:index.html

DATA_FILES = $(wildcard data/*.R)
DATA_RDA = $(DATA_FILES:R=rda)

data/%.rda:data/%.R
	Rscript -e "source('$<')"

index.html:index.Rmd css/custom.css $(DATA_RDA)
	Rscript -e "rmarkdown::render('$<')"

clean:
	rm -f $(DATA_RDA)
	rm -f index.html
