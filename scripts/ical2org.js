const fs = require('fs')
const os = require('os')
const exec = require('child_process').execSync
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
    exec(name)
    return true
  } catch (err) {
    return false
  }
}

const hasIcal2OrgPy = isExecutable('ical2orgpy 2> /dev/null')
if (!hasIcal2OrgPy) {
  exec('pipx install ical2orgpy')
}
function processUrl (url) {
  const events = exec(`curl -s "${url}" | ical2orgpy - -`)
  const cacheFile = `${os.homedir()}/.cache/ical2org`
  exec(`touch "${cacheFile}"`)
  exec(`touch "${OUTFILE}"`)
  const cache = fs
    .readFileSync(cacheFile)
    .toString()
    .trim()
    .split(/\n/)
  return events
    .toString()
    .split(/\n\n\*/g)
    .forEach(async function (event) {
      event = `*${event}`
      const md5hash = exec(`echo "${event}" | md5sum`)
        .toString()
        .trim()
        .split(/\s+/)[0]
      if (cache.indexOf(md5hash) < 0) {
        fs.appendFileSync(cacheFile, `${md5hash}\n`)
        fs.appendFileSync(OUTFILE, `${event}\n\n`)
        return true
      }
      return false
    })
}

if (typeof URL === 'string') {
  processUrl(URL)
} else {
  URL.forEach(function (url) {
    processUrl(url)
  })
}
