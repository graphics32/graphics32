import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const repoRoot = path.resolve(__dirname, '../..');
const examplesDir = path.join(repoRoot, 'Examples');
const targetPublicDir = path.join(repoRoot, 'docs/public/examples');
const outputFile = path.join(__dirname, 'theme/exampleScreenshots.json');

function syncScreenshots() {
  if (!fs.existsSync(examplesDir)) {
    console.error(`Examples directory not found at ${examplesDir}`);
    return;
  }

  if (!fs.existsSync(targetPublicDir)) {
    fs.mkdirSync(targetPublicDir, { recursive: true });
  }

  const screenshots = [];

  function scan(dir) {
    const entries = fs.readdirSync(dir, { withFileTypes: true });
    for (const entry of entries) {
      const fullPath = path.join(dir, entry.name);
      if (entry.isDirectory()) {
        scan(fullPath);
      } else if (entry.isFile() && entry.name.toLowerCase() === 'screenshot.png') {
        const relativePath = path.relative(examplesDir, fullPath);
        // e.g. "Transformation/ImgWarping/screenshot.png"
        const slug = relativePath
          .replace(/[\\/]/g, '_')
          .replace(/[^a-zA-Z0-9_\.-]/g, '');

        const targetFileName = slug;
        const targetFilePath = path.join(targetPublicDir, targetFileName);

        fs.copyFileSync(fullPath, targetFilePath);

        const categoryAndName = relativePath.replace(/\\/g, '/').replace('/screenshot.png', '');
        screenshots.push({
          src: `/examples/${targetFileName}`,
          alt: categoryAndName,
          path: relativePath.replace(/\\/g, '/')
        });
      }
    }
  }

  scan(examplesDir);

  // Sort deterministically by path
  screenshots.sort((a, b) => a.path.localeCompare(b.path));

  fs.mkdirSync(path.dirname(outputFile), { recursive: true });
  fs.writeFileSync(outputFile, JSON.stringify(screenshots, null, 2), 'utf-8');

  console.log(`Synced ${screenshots.length} example screenshots into ${targetPublicDir}`);
}

syncScreenshots();
