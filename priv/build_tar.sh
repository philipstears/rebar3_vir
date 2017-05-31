#!/bin/bash
SOURCEDIR=$(dirname "${BASH_SOURCE[0]}")

build_tar() {
  local app_dir=$1
  local releases_folder=$2
  local git_tag=$3

  local app=$(basename $app_dir)
  local working_dir=$(dirname $app_dir)
  local tar_dir="${app}_$git_tag"
  local tar_name="${tar_dir}.tar.gz"
  local autorun_name="$releases_folder/install-${tar_name%.tar.gz}"

  if [[ -f apps/$app/release-files/pre_tar.sh ]]; then
      echo Running pre-tar script for $app
      apps/$app//release-files/pre_tar.sh
  fi

  echo Building $app Archive...
  rm -f $tar_name

  pushd $working_dir > /dev/null
  mv $app $tar_dir


  echo "In $working_dir and doing tar of $releases_folder/$tar_name $tar_dir"
  tar cfz $releases_folder/$tar_name $tar_dir  || { echo "Tar failed"; exit 1; }
  mv $tar_dir $app
  popd > /dev/null

  if [[ -f apps/$app/release-files/post_tar.sh ]]; then
    echo Running post-tar script for $app
    apps/$app/release-files/post_tar.sh $tar_name $git_tag
  else
    echo "About to create package"
    cat $SOURCEDIR/autoextract.sh $releases_folder/$tar_name > $autorun_name
    chmod +x $autorun_name
    echo done
  fi
  rm $releases_folder/$tar_name
}

build_tar $@
