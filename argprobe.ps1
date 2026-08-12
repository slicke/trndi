param(
    [Parameter(ValueFromRemainingArguments=$true)]
    [string[]]$MakeArgs
)
$extraArgs = if ($MakeArgs.Length -gt 1) { $MakeArgs[1..($MakeArgs.Length - 1)] } else { @() }
Write-Host ("MakeArgs   = [{0}]" -f ($MakeArgs -join ', '))
Write-Host ("extraArgs  = [{0}]" -f ($extraArgs -join ', '))
Write-Host ("args.Count = {0}" -f $args.Count)
Write-Host ("args -contains '-all'      : {0}" -f ($args -contains '-all'))
Write-Host ("extraArgs -contains '-all' : {0}" -f ($extraArgs -contains '-all'))
