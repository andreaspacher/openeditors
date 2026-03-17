# parsers/__init__.py
from .alliedacademies import parse_alliedacademies
from .apa import parse_apa
from .asce import parse_asce
from .cup import parse_cup
from .elife import parse_elife, run_elife
from .elsevier import parse_elsevier, slugify
from .emerald import parse_emerald
from .frontiers import parse_frontiers, fetch_page_scrolling
from .imedpub import parse_imedpub, run_imedpub
from .inderscience import parse_inderscience
from .karger import parse_karger
from .longdom import parse_longdom, run_longdom
from .omics import parse_omics, run_omics
from .peerj import parse_peerj, run_peerj
from .pleiades import parse_pleiades
from .plos import parse_plos, run_plos
from .rsc import parse_rsc
from .sage import parse_sage
from .scirp import parse_scirp, run_scirp
from .scitechnol import parse_scitechnol, run_scitechnol
from .springernature import parse_springernature

STRATEGIES = {
    "apa":    parse_apa,
    "asce":    parse_asce,
    "cup": parse_cup,
    "emerald":      parse_emerald,
    "elife":        parse_elife,
    "inderscience":  parse_inderscience,
    "pleiades":     parse_pleiades,
    "rsc":       parse_rsc,
    "springernature":  parse_springernature,
    "frontiers":      parse_frontiers,
    "peerj":        parse_peerj,
    "sage":     parse_sage,
    "elsevier":     parse_elsevier,
    "karger":     parse_karger,   
    "plos": parse_plos,
    "alliedacademies": parse_alliedacademies,
    "omics": parse_omics,
    "longdom": parse_longdom,
    "scitechnol": parse_scitechnol,
    "imedpub": parse_imedpub,
    "scirp": parse_scirp
}