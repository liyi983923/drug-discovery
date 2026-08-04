# Drug Discovery & Development Capability Profile

An R-generated 16:9 infographic showing the drug discovery and development
pipeline together with a stage-by-stage capability profile scored from 1–10.

![Drug discovery capability profile](drug_discovery_pipeline_4K.png)

## Outputs

- `drug_discovery_pipeline_4K.png` — UHD 4K PNG (3840 × 2160)
- `drug_discovery_pipeline.svg` — scalable SVG export
- `drug_discovery_pipeline.R` — reproducible R source
- `drug_discovery_pipeline_reference.png` — source process-flow artwork used by the hybrid R composition

## Capability scores

| Stage | Score |
|---|---:|
| Target Discovery | 9/10 |
| Molecular Design | 7/10 |
| Preclinical Development | 7/10 |
| Clinical Development | 7/10 |
| Marketed Medicine | 2/10 |

## Regenerate

The script uses `grid`, `ragg`, `svglite`, `systemfonts`, `showtext`, and
`magick`. From the repository directory, run:

```bash
Rscript drug_discovery_pipeline.R
```

Roboto Condensed font files and their license are included under `fonts/`.

