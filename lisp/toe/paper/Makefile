# combine all the data tables into one
# get the refs going
# comment at end that we dont explor other things cant we dont, as yet, have recognized a data feature
#    that guides that deciision. however, we have clues.
# move ekrem's prototype stuff into my list

Src=toe-v2


all : dirs tex bib  tex tex done

one : dirs tex done 

done :
	@printf "\n\n\n==============================================\n"
	@printf       "see output in $(HOME)/tmp/$(Src).pdf\n"
	@printf       "==============================================\n\n\n"
	@printf "\n\nWarnings (may be none):\n\n"
	grep arning $(HOME)/tmp/${Src}.log

dirs : 
	- [ ! -d $(HOME)/tmp ] && mkdir $(HOME)/tmp

tex :
	- pdflatex -output-directory=$(HOME)/tmp $(Src)
	
bib : 
	- bibtex $(HOME)/tmp/$(Src)

