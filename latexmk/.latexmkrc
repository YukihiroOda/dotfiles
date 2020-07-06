#!/usr/bin/env perl
$latex = 'lualatex %O -synctex=1 %S -halt-on-error';
$latex_silent = 'lualatex %O -synctex=1 %S -halt-on-error -interaction=batchmode';
$pdflatex = 'pdflatex %O -synctex=1 %S -halt-on-error';
$lualatex = 'lualatex %O -synctex=1 %S -halt-on-error';
$xelatex = 'xelatex %O -synctex=1 %S -halt-on-error';
$biber = 'biber %O --bblencoding=utf8 -u -U --output_safechars %B';
$bibtex = 'upbibtex %O %B';
$makeindex = 'upmendex %O -o %D %S';
$max_repeat = 5;
$dvipdf = 'dvipdfmx %O -o %D %S';
$dvips = 'dvips %O -z -f %S | convbkmk -u > %D';
$ps2pdf = 'ps2pdf.exe %O %S %D';
$pdf_mode = 3;

if ($^O eq 'MSWin32') {
  if (-f 'C:/Program Files/SumatraPDF/SumatraPDF.exe') {
    $pdf_previewer = '"C:/Program Files/SumatraPDF/SumatraPDF.exe" -reuse-instance';
  }
  elsif (-f 'C:/Program Files (x86)/SumatraPDF/SumatraPDF.exe') {
    $pdf_previewer = '"C:/Program Files (x86)/SumatraPDF/SumatraPDF.exe" -reuse-instance';
  }
  elsif (-f "~\AppData\Local\SumatraPDF\SumatraPDF.exe") {
      $pdf_previewer = '"C:/Program Files (x86)/SumatraPDF/SumatraPDF.exe" -reuse-instance';
  }
  else {
    $pdf_previewer = 'texworks';
  }
}
else {
    if (-f "/mnt/c/SumatraPDF/SumatraPDF.exe"){ # for wsl settings
	$pdf_previewer = '"/mnt/c/SumatraPDF/SumatraPDF.exe" -reuse-instance';
    }
    else {
	$pdf_previewer = 'evince';
    }
}
