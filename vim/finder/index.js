/**
 * Usage: node index.js --package <clap|denite|coc.nvim>
 */
const fs = require('fs');
const fetch = require('isomorphic-fetch');
const argv = require('minimist')(process.argv.slice(2), {
  default: {
    package: 'clap'
  },
  string: [
    'package'
  ]
});
const manifest = require('./manifest.json');

const PACKAGES = ['clap', 'denite', 'coc.nvim'];

if (PACKAGES.indexOf(argv.package) < 0) {
  throw new Error(`Unknown package: ${argv.package}`);
}

const localizeFile = pack => file => `${process.env.HOME}/dotfiles/${file.split(new RegExp(`/${pack}/`))[1]}`;

// Update files that need to be updated and download files that are required
// for this version:
manifest[argv.package].update.concat(manifest[argv.package].clean ? manifest[argv.package].clean : [])
  .forEach(file => fetch(file)
    .then(res => res.text())
    .then(contents => fs.writeFileSync(localizeFile(argv.package)(file), contents))
  );
// Clean files unused by current package:
Object.keys(manifest)
  .filter(key => key !== argv.package)
  .forEach(currentKey => {
    if (Object.prototype.hasOwnProperty.call(manifest[currentKey], 'clean')) {
      manifest[currentKey].clean
        .map(localizeFile(currentKey))
        .filter(fs.existsSync)
        .forEach(fs.unlinkSync);
    }
  });
