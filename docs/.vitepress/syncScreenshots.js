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

        const relativeFolder = path.dirname(relativePath).replace(/\\/g, '/');
        const folderName = path.basename(relativeFolder);

        // Read metadata from index.md if present
        const indexPath = path.join(dir, 'index.md');
        let concept = '';
        let techniques = '';
        let api = [];

        if (fs.existsSync(indexPath)) {
          try {
            const indexContent = fs.readFileSync(indexPath, 'utf-8');
            const match = indexContent.match(/^---\r?\n([\s\S]*?)\r?\n---/);
            if (match) {
              const yamlStr = match[1];

              const conceptMatch = yamlStr.match(/^Concept:\s*(.*)$/m);
              if (conceptMatch) {
                concept = conceptMatch[1].trim().replace(/^["']|["']$/g, '');
              }

              const techMatch = yamlStr.match(/^Techniques:\s*(.*)$/m);
              if (techMatch) {
                techniques = techMatch[1].trim().replace(/^["']|["']$/g, '');
              }

              const apiMatches = yamlStr.matchAll(/^\s*-\s*["']?\[\[(.*?)\]\]["']?/gm);
              for (const apiMatch of apiMatches) {
                api.push(apiMatch[1].trim());
              }
            }
          } catch (e) {
            console.error(`Failed to parse index.md at ${indexPath}:`, e);
          }
        }

        screenshots.push({
          src: `/examples/${targetFileName}`,
          alt: folderName,
          name: folderName,
          path: relativePath.replace(/\\/g, '/'),
          folder: relativeFolder,
          concept,
          techniques,
          api
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
