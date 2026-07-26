#Requires -RunAsAdministrator

$ErrorActionPreference = 'Stop'

$data = (Resolve-Path 'D:\PostgreSQL_Recuperado\18\data').Path
if ($data -ne 'D:\PostgreSQL_Recuperado\18\data') {
  throw "Ruta inesperada: $data"
}

$serviceName = 'postgresql-x64-18-recovered'
$pgCtl = 'C:\Program Files\PostgreSQL\18\bin\pg_ctl.exe'

# Detiene la instancia manual, si esta activa, antes de ocupar el mismo puerto.
& $pgCtl status -D $data *> $null
if ($LASTEXITCODE -eq 0) {
  & $pgCtl stop -D $data -m fast -w -t 120
  if ($LASTEXITCODE -ne 0) { throw 'No se pudo detener la instancia manual.' }
}

# Permiso minimo operativo para WAL, checkpoints, tablas y logs.
& icacls.exe $data /grant '*S-1-5-20:(OI)(CI)M' /T /C /Q
if ($LASTEXITCODE -ne 0) { throw 'No se pudo asignar permiso Modificar a NetworkService.' }

if (-not (Get-Service $serviceName -ErrorAction SilentlyContinue)) {
  & $pgCtl register -N $serviceName -D $data -S auto -U 'NT AUTHORITY\NetworkService'
  if ($LASTEXITCODE -ne 0) { throw "No se pudo registrar $serviceName." }
}

Start-Service $serviceName
$service = Get-Service $serviceName
if ($service.Status -ne 'Running') { throw "$serviceName no quedo en ejecucion." }

& 'C:\Program Files\PostgreSQL\18\bin\pg_isready.exe' -h localhost -p 5433
if ($LASTEXITCODE -ne 0) { throw 'El servicio inicio, pero PostgreSQL no acepta conexiones en 5433.' }

$service | Select-Object Name, Status, StartType
