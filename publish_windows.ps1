# Powershell script for quarto pubs and gh-pages
# ./publish_windows.ps1 FORMAT FILE
# FORMAT = book, slides, or slides_wide
# FILE = all or filename

$FORMAT=$args[0]
$FILE=$args[1]

if ( $FORMAT -eq "book" )
{
  Write-Host ""
  Write-Host  "Publishing all chapters to gh-pages"
  Write-Host  ""
  Copy-Item _quarto_book.yml _quarto.yml
  quarto publish gh-pages --no-browser 
  git restore _quarto.yml
  Remove-Item -r _book
}

 
if ($FORMAT -eq "slides" )
{
  Write-Host  ""
  Write-Host  "Publishing $FILE to standard slides on quarto-pub"
  Write-Host  ""
  Copy-Item _quarto_slides.yml _quarto.yml
  quarto publish quarto-pub $FILE --no-browser
  git restore _quarto.yml
}
 
if ($FORMAT -eq "slides_wide" )
{
  Write-Host  ""
  Write-Host  "Publishing $FILE to wide slides on quarto-pub"
  Write-Host  ""
  Copy-Item _quarto_slides_wide.yml _quarto.yml
  quarto publish quarto-pub $FILE --no-browser
  git restore _quarto.yml
}
