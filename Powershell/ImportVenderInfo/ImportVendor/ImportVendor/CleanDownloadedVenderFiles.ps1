#
# CleanDownloadedVenderFiles.ps1
#
Clear-Host

Clear-Content "C:\users\Torben\Downloads\IBM Corporation_v2.csv"

$pat = "\(([^)]*)\)"


$reader = [system.io.file]::OpenText("C:\users\Torben\Downloads\IBM Corporation.csv")

while ($null -ne ($line = $reader.readline()))
{
  $text = $line
 
  $value = [regex]::match($text, $pat).Groups[1].value.trim()
  $value1 = $value -replace ",", "" #| Set-Content -Path "C:\users\Torben\Downloads\IBM Corporation_v2.csv"

  $text = $text -replace $value , $value1

  add-content "C:\users\Torben\Downloads\IBM Corporation_v2.csv" $text


}