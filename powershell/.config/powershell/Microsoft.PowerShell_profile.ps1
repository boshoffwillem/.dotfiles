# Starship
Invoke-Expression (&starship init powershell)
$ENV:STARSHIP_CONFIG = "/home/willemboshoff/.config/starship.toml"

# Better icons
Import-Module -Name Terminal-Icons

# PSReadLine
Import-Module PSReadLine
Set-PSReadLineOption -PredictionSource History
Set-PSReadLineOption -PredictionViewStyle ListView
Set-PSReadLineOption -EditMode Windows

Set-Alias -Name grep -Value rg
Set-Alias -Name sed -Value sd
Set-Alias -Name n -Value neovide
Set-Alias -Name dotnet -Value '~/.dotnet/dotnet'
