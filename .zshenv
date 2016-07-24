# Get environment variables that are intended for both bash and zsh
if [ -f ${HOME}/.profile ]; then
    source ${HOME}/.profile
fi
