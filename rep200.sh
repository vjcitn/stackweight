#!/bin/bash
for PK in BiocVersion BiocGenerics S4Vectors IRanges Biobase zlibbioc AnnotationDbi XVector BiocParallel GenomeInfoDb DelayedArray GenomicRanges SummarizedExperiment limma Biostrings Rsamtools biomaRt annotate genefilter GenomicAlignments Rhtslib graph rtracklayer edgeR GenomicFeatures BiocFileCache DESeq2 Rhdf5lib geneplotter rhdf5 preprocessCore multtest qvalue Rgraphviz RBGL fgsea GOSemSim clusterProfiler BSgenome affyio affy DOSE impute HDF5Array enrichplot ProtGenerics ensembldb GEOquery ShortRead VariantAnnotation DelayedMatrixStats ComplexHeatmap AnnotationFilter sva GSEABase AnnotationHub KEGGREST SingleCellExperiment interactiveDisplayBase beachmat scater biovizBase DESeq KEGGgraph pathview BiocSingular vsn BiocStyle BiocNeighbors phyloseq pcaMethods biomformat Gviz topGO aroma.light biocViews AnnotationForge Category ExperimentHub tximport EBImage OrganismDbi EDASeq ggtree GOstats illuminaio ConsensusClusterPlus TCGAbiolinks DNAcopy treeio siggenes bumphunter GSVA scran monocle mixOmics mzR ggbio minfi gcrma oligoClasses apeglm flowCore MSnbase EnhancedVolcano oligo Rsubread mzID affxparser dada2 regioneR maftools marray graphite batchelor seqLogo snpStats affyPLM gdsfmt ReactomePA cytolib methylumi xcms MultiAssayExperiment DirichletMultinomial goseq ChIPseeker Glimma DECIPHER lumi flowWorkspace flowViz SNPRelate globaltest RProtoBufLib ROC MassSpecWavelet BiocCheck msa bsseq ncdfFlow MAST systemPipeR lpsymphony SingleR ggcyto gage GenomicDataCommons flowClust IHW ReportingTools ChIPpeakAnno flowStats FlowSOM openCyto GenomicFiles DropletUtils missMethyl destiny CytoML survcomp RUVSeq DSS beadarray DiffBind ballgown tximeta DMRcate microbiome wateRmelon TFBSTools BeadDataPackR slingshot CNEr DEXSeq NOISeq ChemmineOB ArrayExpress metagenomeSeq bamsignals zinbwave RTCGA fastseg simpleaffy AUCell ChAMP RaggedExperiment RCy3 TCGAutils baySeq karyoploteR minet Heatplus arrayQualityMetrics rGADEM STRINGdb ropls chipseq BiocSklearn RTCGAToolbox
do
  echo $PK ----
  R --silent -e "system.time(library('$PK'))"
done
