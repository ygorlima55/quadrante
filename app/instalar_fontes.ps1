# Instala a fonte Inter (pasta fontes/ do Quadrante, licença SIL OFL) só para o usuário do Windows, sem pedir administrador.
# Os gráficos do tema Modelo usam Inter e Inter Black; sem ela o R usa a Segoe UI.
param([string]$Pasta = (Join-Path (Split-Path -Parent $PSScriptRoot) "fontes"))
$dst = Join-Path $env:LOCALAPPDATA "Microsoft\Windows\Fonts"
New-Item -ItemType Directory -Force $dst | Out-Null
$nomes = @{ "Inter-400.ttf" = "Inter Regular (TrueType)"; "Inter-500.ttf" = "Inter Medium (TrueType)"; "Inter-600.ttf" = "Inter SemiBold (TrueType)"; "Inter-700.ttf" = "Inter Bold (TrueType)"; "Inter-900.ttf" = "Inter Black (TrueType)" }
Add-Type -Namespace Q -Name F -MemberDefinition '[DllImport("gdi32.dll", CharSet=CharSet.Unicode)] public static extern int AddFontResource(string f);'
foreach ($f in $nomes.Keys) {
  $o = Join-Path $Pasta $f
  if (-not (Test-Path $o)) { continue }
  if (-not (Test-Path (Join-Path $dst $f))) { Copy-Item $o (Join-Path $dst $f) -Force }   # já instalada (e em uso): não copia de novo
  New-ItemProperty -Path "HKCU:\Software\Microsoft\Windows NT\CurrentVersion\Fonts" -Name $nomes[$f] -Value (Join-Path $dst $f) -PropertyType String -Force | Out-Null
  [Q.F]::AddFontResource((Join-Path $dst $f)) | Out-Null   # vale já nesta sessão, sem reiniciar
}
Write-Output "Fonte Inter instalada para este usuário."
