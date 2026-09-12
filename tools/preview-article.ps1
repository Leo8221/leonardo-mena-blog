param([Parameter(Mandatory = $true)][string]$Article)
$ErrorActionPreference = 'Stop'
& (Join-Path $PSScriptRoot 'render-quarto.ps1') -QuartoArgs @('preview', $Article, '--profile', 'editor')
