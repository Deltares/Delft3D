# Base image from harbor
FROM containers.deltares.nl/delft3d_latex/latex-builder

COPY ./* /workspace
COPY ./images /workspace/images

# Use a shell form of CMD to invoke latexmk on .tex file passed at runtime
CMD latexmk -pdf -outdir=/workspace/output /workspace/minio-tool.tex
