# Package Signing Setup for CI

This document explains how to set up GPG signing for DEB and RPM packages in the GitHub Actions CI workflow.

## Prerequisites

You need a GPG key pair for signing packages. If you don't have one, follow the generation steps below.

## Step 1: Generate GPG Key (if needed)

Run these commands on your local machine:

```bash
# Generate a new GPG key (use RSA 4096-bit)
gpg --full-generate-key

# When prompted:
# - Select: (1) RSA and RSA
# - Key size: 4096
# - Expiration: 0 (key does not expire) or set your preferred expiration
# - Real name: Trndi Release Bot (or your name)
# - Email: your-email@example.com
# - Passphrase: Choose a strong passphrase
```

## Step 2: Export Your GPG Key

```bash
# List your keys to find the key ID
gpg --list-secret-keys --keyid-format=long

# Output will look like:
# sec   rsa4096/ABCD1234EFGH5678 2025-01-01 [SC]
#       The part after rsa4096/ is your KEY_ID

# Export the private key (ASCII armored)
gpg --armor --export-secret-keys YOUR_KEY_ID > gpg-private-key.asc

# Export the public key (for users to verify signatures)
gpg --armor --export YOUR_KEY_ID > gpg-public-key.asc
```

## Step 3: Add Secrets to GitHub

1. Go to your repository on GitHub
2. Navigate to **Settings** → **Secrets and variables** → **Actions**
3. Click **New repository secret** and add:

   - **Name:** `GPG_PRIVATE_KEY`
   - **Value:** Paste the entire contents of `gpg-private-key.asc`

4. Add another secret:
   - **Name:** `GPG_PASSPHRASE`
   - **Value:** The passphrase you used when creating the GPG key

## Step 4: Publish Your Public Key

Users need your public key to verify package signatures. You can:

### Option A: Upload to a Key Server
```bash
gpg --keyserver keyserver.ubuntu.com --send-keys YOUR_KEY_ID
gpg --keyserver keys.openpgp.org --send-keys YOUR_KEY_ID
```

### Option B: Add to Your Repository
```bash
# Copy the public key to your repo
cp gpg-public-key.asc /path/to/trndi2/doc/trndi-signing-key.asc
git add doc/trndi-signing-key.asc
git commit -m "Add package signing public key"
git push
```

### Option C: Both (Recommended)
Do both for maximum distribution.

## Step 5: Document for Users

Add instructions to your README or installation guide:

### For DEB Packages (Ubuntu/Debian)

```bash
# Import the Trndi signing key
gpg --import trndi-signing-key.asc

# Verify the package signature (using the separate .asc signature file)
gpg --verify trndi_*.deb.asc trndi_*.deb
```

### For RPM Packages (Fedora/RHEL/OpenSUSE)

```bash
# Import the Trndi signing key
sudo rpm --import https://raw.githubusercontent.com/slicke/trndi/main/doc/trndi-signing-key.asc

# Verify the package signature
rpm --checksig trndi-*.rpm
```

## How It Works

The CI workflow automatically signs packages during the build process:

1. **Checks for GPG key:** If `GPG_PRIVATE_KEY` secret exists, it proceeds with signing
2. **Imports the key:** Loads your private key into GPG in the CI environment
3. **Extracts key ID:** Automatically determines the GPG key ID from the imported key
4. **Configures RPM:** Sets up `~/.rpmmacros` with the key ID and GPG settings for RPM signing
5. **Signs DEB:** Creates a detached GPG signature (`.deb.asc` file) for the Debian package using `gpg --detach-sign`
6. **Signs RPM:** Uses `rpm --addsign` (with passphrase piped in) to sign the RPM package (signature embedded)
7. **Includes public key:** Copies `doc/trndi-signing-key.asc` to artifacts if it exists
8. **Continues normally:** If no GPG key is configured, packages are built unsigned (no errors)

The signing happens in the Linux build matrix for both amd64 and arm64 architectures, right after the `fpm` packaging step and before moving files to the `artifacts/` directory.

**Note:** DEB packages use detached signatures (separate `.asc` files) while RPM packages have embedded signatures.

## Security Notes

- **Never commit** your private key or passphrase to the repository
- The private key is only stored in GitHub Secrets (encrypted)
- Consider using a dedicated signing key separate from your personal GPG key
- Set an expiration date on your key and rotate it periodically
- Revoke the key immediately if it's compromised

## Verifying Signatures

After the workflow runs, you can verify signatures:

```bash
# For DEB (using the detached .asc signature file)
gpg --import trndi-signing-key.asc
gpg --verify trndi_*.deb.asc trndi_*.deb

# For RPM
rpm --checksig trndi-*.rpm
```

Successful verification means the packages haven't been tampered with and came from you.

## Troubleshooting

### "gpg: no valid OpenPGP data found"
- Check that you copied the entire private key including `-----BEGIN PGP PRIVATE KEY BLOCK-----` and `-----END PGP PRIVATE KEY BLOCK-----`

### "gpg: signing failed: Inappropriate ioctl for device"
- The workflow uses `--batch` mode and pipes the passphrase to avoid this issue

### RPM signing fails
- Ensure `GPG_PASSPHRASE` is set correctly
- Check that the GPG key has signing capability (not just encryption)

### Can't find the signing key or package files in GitHub releases
- Click **"Show all X assets"** at the bottom of the release notes to expand and see all files
- GitHub collapses the asset list by default if there are many files
- The signing key file is named `trndi-signing-key.asc`

## Key Rotation

When you need to rotate keys:

1. Generate a new GPG key (Step 1)
2. Update both secrets in GitHub (Step 3)
3. Publish the new public key (Step 4)
4. Consider signing one release with both keys during transition
5. Announce the key change to users
