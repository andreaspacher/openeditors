# Open Editors

## Purpose

Webscraping data about scholarly journals' editors and editorial boards.

For methodical details and more information, see [this preprint at *SocArXiv*](https://doi.org/10.31235/osf.io/jvzq7).

## Results

### Basic Data

Data about ca. **594.580** editorial positions at **7.352** journals from **26** publishers were found. 

To see the results, see the `Output`-folder, or browse through the dataset at https://openeditors.ooir.org/. 

In the `Output/2022-Scraping`-folder, the data were split into two files to avoid documents of over 100 MB (`editors1.csv` and `editors2.csv`).

A parquet-file (`editors.parquet`) offers a single document.

Note that the `Output/2021-Scraping`-folder include files with `*_ror.csv`. They contain [ROR](https://ror.org/)-identifiers, while the ones with `*_ror_countries.csv` have yet another column with automatically extracted country names. (I plan to extend those data to the files in `Output/2022-Scraping` in the near future.)


## Data Sample

The current version contains data from journals pertaining to the following 21 (non-predatory) publishers:

- American Psychological Association (APA)
- American Society of Chemical Engineers (ASCE)
- BioMedCentral
- Brill
- Cambridge University Press (CUP)
- eLife
- Elsevier
- Emerald
- Frontiers
- Hindawi
- IGI Global
- Inderscience
- John Benjamins
- Karger
- Multidisciplinary Digital Publishing Institute (MDPI)
- PeerJ
- Pleiades
- Public Library of Science (PLOS)
- Royal Society of Chemistry (RSC)
- SAGE
- Springer Nature

### Predatory Publishers

The five predatory publishers in the sample are:

- Allied Academies (seems to belong to OMICS)
- iMedPub (seems to belong to OMICS)
- Longdom (seems to belong to OMICS)
- SCIRP
- SciTechnol (seems to belong to OMICS)

### Journal URLs

The URLs to the journal websites (as visible in the `Data`-folder) stems from https://github.com/andreaspacher/academic-publishers.

## Related Projects

Jan Kamlah and Philipp Zumstein have scraped editors from Springer, Wiley & Elsevier with Python at https://github.com/JKamlah/scrape-editorial-board.
