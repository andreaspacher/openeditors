# Open Editors

## Purpose

Webscraping data about scholarly journals' editors and editorial boards.

For methodical details and more information, see [this preprint at *SocArXiv*](https://doi.org/10.31235/osf.io/jvzq7).

## Results

Data about ca. **480.000** editorial positions at **6.090** journals from **17** publishers were found. 

In addition, the scripts scraped further data about **19.487** editorial positions at **752** journals from **5** predatory publishers.

To see the results, see the `Output`-folder, or browse through the dataset at https://openeditors.ooir.org/. 

In the `Output`-folder, the data from the 17 non-predatory publishers were split into two files to avoid documents of over 100 MB:
- `editors1.csv` contains editor data from APA, ASCE, Brill, CUP, Elsevier, Emerald, and Frontiers;
- `editors2.csv` contains editor data from Hindawi, IGI Global, Inderscience, John Benjamins, Karger, MDPI, Pleiades, PLOS, RSC, and SAGE.
- The same filenames with `*_ror.csv` additionally contain [ROR](https://ror.org/)-identifiers, while the ones with `*_ror_countries.csv` have yet another column with automatically extracted country names.
- `editors_predatory.journals.csv` contain the results from the 5 predatory publishers.

## Data Sample

The current version contains data from journals pertaining to the following 17 (non-predatory) publishers:

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

The five predatory publishers in the sample are:

- Allied Academies (seems to belong to OMICS)
- iMedPub (seems to belong to OMICS)
- Longdom (seems to belong to OMICS)
- SCIRP
- SciTechnol (seems to belong to OMICS)

The URLs to the journal websites (as visible in the `Data`-folder) stems from https://github.com/andreaspacher/academic-publishers.

## Related Projects

Jan Kamlah and Philipp Zumstein have scraped editors from Springer, Wiley & Elsevier with Python at https://github.com/JKamlah/scrape-editorial-board.
