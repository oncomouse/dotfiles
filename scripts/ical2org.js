const fs = require('fs')
const os = require('os')
const { execSync } = require('child_process')
const { createHash } = require('node:crypto')
const URL = require('./url')

const OUTFILE = resolveTilde('~/SeaDrive/My Libraries/Todo/inbox.org')

function resolveTilde (filePath) {
  if (!filePath || typeof filePath !== 'string') {
    return ''
  }
  // '~/folder/path' or '~' not '~alias/folder/path'
  if (filePath.startsWith('~/') || filePath === '~') {
    return filePath.replace('~', os.homedir())
  }
  return filePath
}

async function isExecutable (name) {
  try {
    execSync(name)
    return true
  } catch (err) {
    return false
  }
}

const hasIcal2OrgPy = isExecutable('ical2orgpy 2> /dev/null')
if (!hasIcal2OrgPy) {
  execSync('pipx install ical2orgpy')
}
function processUrl (url) {
  const events = execSync(`curl -s "${url}" | ical2orgpy - -`)
  const cacheFile = `${os.homedir()}/.cache/ical2org`
  if (!fs.existsSync(cacheFile)) {
    execSync(`touch "${cacheFile}"`)
  }
  if (!fs.existsSync(OUTFILE)) {
    execSync(`touch "${OUTFILE}"`)
  }
  const cache = fs
    .readFileSync(cacheFile)
    .toString()
    .trim()
    .split(/\n/)
  return events
    .toString()
    .split(/\n\n\*/g)
    .filter(function (event) {
      event = `*${event}`
      const hash = createHash('md5')
      hash.update(event)
      const md5hash = hash.copy().digest('hex')
      if (cache.indexOf(md5hash) < 0) {
        fs.appendFileSync(cacheFile, `${md5hash}\n`)
        fs.appendFileSync(OUTFILE, `${event}\n\n`)
        return true
      }
      return false
    })
}

;(typeof URL === 'string' ? [URL] : URL).forEach(processUrl)
