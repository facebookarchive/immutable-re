const ChildProcess = require('child_process');
const Fs = require('fs');

const exec = cmd => new Promise(
  (resolve, reject) => {
    ChildProcess.exec(cmd, (err, stdout, stderr) => {
      if (err != null) {
        reject(err);
      } else {
        resolve({ stdout, stderr });
      }
    });
  }
);

const readFile = (fileName, encodingType) => new Promise (
  (resolve, reject) => {
    Fs.readFile(fileName, encodingType, (err, data) => {
      if (err != null) { reject(err); } else { resolve(data); }
    });
  }
);

const opamFile = "opam";
const metaFile = "META";
const packageJSONFile = "package.json";

const main = async () => {
  const packageJSON = await readFile (packageJSONFile, "UTF-8");
  const version = JSON.parse(packageJSON).version;

  await exec(`git add ${ opamFile } ${ metaFile } ${ packageJSONFile }`)
  await exec(`git commit -m "Version ${ version }"`);
  await exec(`git tag -a ${ version } -m "Version ${ version }."`);
  await exec('git push "git@github.com:facebookincubator/immutable-re.git"');
  await exec(`git push "git@github.com:facebookincubator/immutable-re.git" tag ${version}`);
  await exec(`opam-publish prepare https://github.com/facebookincubator/immutable-re/archive/${ version }.tar.gz`);
  await exec(`opam-publish submit immutable.${ version }`);
  await exec('npm publish');
};


main ();
