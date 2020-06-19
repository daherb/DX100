# DX100

Generates random voices for the DX100 FM synthesizer.

## Install

Run `stack install` (requires the Haskell stack)

## Run

Run `dx100 your_voice.syx` to generate a voice.

On Linux you can use `amidi` to send the SYX file to the synthesizer
e.g. `amidi -p "hw:2,0,0" --send your_voice.syx`

## Disclaimer

It can take several attempts to get a reasonable voice
