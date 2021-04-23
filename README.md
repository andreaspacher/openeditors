# Open Editors

## Purpose

Webscraping data about scholarly journals' editors and editorial boards.

For methodical details and more information, see [this preprint at *SocArXiv*](https://doi.org/10.31235/osf.io/jvzq7).

## Results

Data about **478.512** editorial positions at **6.090** journals from **17** publishers were found. 

To see the results, search through the dataset at https://openeditors.ooir.org/, or download the files from the `Output`-folder.

In the `Output`-folder, the data was split into two files to avoid documents of over 100 MB:
- `editors1.csv` contains editor data from APA, ASCE, Brill, CUP, Elsevier, Emerald, and Frontiers;
- `editors2.csv` contains editor data from Hindawi, IGI Global, Inderscience, John Benjamins, Karger, MDPI, Pleiades, PLOS, RSC, and SAGE.

## Data Sample

The current version contains data from journals pertaining to the following 17 publishers:

- American Psychological Association (APA)
- American Society of Chemical Engineers (ASCE)
- Brill
- Cambridge University Press (CUP)
- Elsevier
- Emerald
- Frontiers
- Hindawi
- IGI Global
- Inderscience
- John Benjamins
- Karger
- Multidisciplinary Digital Publishing Institute (MDPI)
- Pleiades
- Public Library of Science (PLOS)
- Royal Society of Chemistry (RSC)
- SAGE

The URLs to the journal websites (as visible in the `Data`-folder) stems from https://github.com/andreaspacher/academic-publishers.

## Related Projects

Jan Kamlah and Philipp Zumstein have scraped editors from Springer, Wiley & Elsevier with Python at https://github.com/JKamlah/scrape-editorial-board.
