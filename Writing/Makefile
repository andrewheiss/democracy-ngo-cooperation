# Put this Makefile in your project directory---i.e., the directory
# containing the paper you are writing. Assuming you are using the
# rest of the toolchain here, you can use it to create .html, .tex,
# and .pdf output files (complete with bibliography, if present) from
# your markdown file. 
# -	Change the paths at the top of the file as needed.
# -	Using `make` without arguments will generate html, tex, pdf, docx,  
# 	and odt output files from all of the files with the designated 
#	markdown extension. The default is `.md` but you can change this. 
# -	You can specify an output format with `make tex`, `make pdf`,  
# - 	`make html`, `make odt`, or `make docx`
# -	Doing `make clean` will only remove all .tex, .html, .pdf, .odt,
#	and .docx files in your working directory *that have the same name
#	as your Markdown files*. Other files with these extensions will be safe.
# -	If wanted, remove the automatic call to `clean` to rely on Make's
#  timestamp checking. However, if you do this, you'll need to add all the
#  document's images, etc. as dependencies, which means it might be easier
#  to just clean and delete everything every time you rebuild.

# Markdown extension (e.g. md, markdown, mdown).
MEXT = md

# All markdown files in the working directory
SRC = $(wildcard *.$(MEXT))
BASE = $(basename $(SRC))

# Location of Pandoc support files.
PREFIX = /Users/andrew/.pandoc

# Location of your working bibliography file
BIB_FILE = /Users/andrew/Dropbox/Readings/Papers.bib

# CSL stylesheet (located in the csl folder of the PREFIX directory).
CSL = chicago-fullnote-bibliography
# CSL = chicago-syllabus-no-bib
# CSL = apa


PDFS=$(SRC:.md=.pdf)
HTML=$(SRC:.md=.html)
TEX=$(SRC:.md=.tex)
ODT=$(SRC:.md=.odt)
DOCX=$(SRC:.md=.docx)
BIB=$(SRC:.md=.bib)

all:	$(PDFS) $(HTML) $(TEX) $(ODT) $(DOCX)

pdf:	clean $(PDFS)
html:	clean $(HTML)
tex:	clean $(TEX)
odt:	clean $(ODT)
docx:	clean $(DOCX)
bib:	$(BIB)

%.html:	%.md
	replace_includes $< | pandoc -r markdown+simple_tables+table_captions+yaml_metadata_block -w html -S --template=$(PREFIX)/templates/html.template --css=$(PREFIX)/marked/kultiad-serif.css --filter pandoc-citeproc --csl=$(PREFIX)/csl/$(CSL).csl --bibliography=$(BIB_FILE) -o $@

%.odt:	%.md
	replace_includes $< | pandoc -r markdown+simple_tables+table_captions+yaml_metadata_block -w odt -S --template=$(PREFIX)/templates/odt.template --filter pandoc-citeproc --csl=$(PREFIX)/csl/$(CSL).csl --bibliography=$(BIB_FILE) -o $@

%.tex:	%.md
	./vc
	@# Add `-V vc` and change pagestyle to athgit to add version control support
	replace_includes $< | pandoc -r markdown+simple_tables+table_captions+yaml_metadata_block -w latex -s -S --latex-engine=xelatex --template=$(PREFIX)/templates/xelatex.template --filter pandoc-citeproc --csl=$(PREFIX)/csl/$(CSL).csl --bibliography=$(BIB_FILE) -V chapterstyle=hikma-article -V pagestyle=athgit -V vc -o $@

%.pdf:	%.md
	./vc
	@# Add `-V vc` and change pagestyle to athgit to add version control support
	replace_includes $< | pandoc -r markdown+simple_tables+table_captions+yaml_metadata_block -s -S --latex-engine=xelatex --template=$(PREFIX)/templates/xelatex.template --filter pandoc-citeproc --csl=$(PREFIX)/csl/$(CSL).csl --bibliography=$(BIB_FILE) -V chapterstyle=hikma-article -V pagestyle=athgit -V vc -o $@

%.docx:	%.odt
	/Applications/LibreOffice.app/Contents/MacOS/soffice --invisible --convert-to docx $<

%.bib: %.md
	bib_extract --bibtex_file $(BIB_FILE) $< $@

clean:
	rm -f $(addsuffix .html, $(BASE)) $(addsuffix .pdf, $(BASE)) $(addsuffix .tex, $(BASE)) $(addsuffix .odt, $(BASE)) $(addsuffix .docx, $(BASE))
