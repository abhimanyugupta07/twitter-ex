## ConfigLoader

[![Build Status](https://travis-ci.org/alexflav23/twitter-ex.svg?branch=master)](https://travis-ci.org/alexflav23/twitter-ex) [![Coverage Status](https://coveralls.io/repos/github/alexflav23/twitter-ex/badge.svg?branch=master)](https://coveralls.io/github/alexflav23/twitter-ex?branch=master)

### Assumptions

The following set of assumptions were made in addition to the original
instructions to fully define the parsing setting of the config.

- A setting is defined as $key<$override>
- A setting $key cannot contain line endings.
- A setting $key cannot contain delimiting characters("<", ">").
- A setting $override cannot contain line endings.
- A setting $override cannot contain delimiting characters("<", ">").