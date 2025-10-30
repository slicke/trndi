# Installing with NuGet
```powershell
# One-time setup: Add GitHub Packages as a source (requires a GitHub personal access token with read:packages scope)
dotnet nuget add source https://nuget.pkg.github.com/slicke/index.json --name github --username YOUR_GITHUB_USERNAME --password YOUR_GITHUB_PAT

# Install Trndi
nuget install trndi
```

Or install the `.nupkg` file directly with Chocolatey:
```powershell
# Download the .nupkg from releases, then:
choco install trndi.VERSION.nupkg
```