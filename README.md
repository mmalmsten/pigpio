# Pigpio - Using Erlang for GPIO on a Raspberry Pi

Using Erlang for GPIO on a Raspberry Pi with [http://abyz.me.uk/rpi/pigpio/](http://abyz.me.uk/rpi/pigpio/)

This library is still very much in a ⚠️ WORK IN PROGRESS ⚠️ state. I'm currently integrating it in [one of my pet projects](https://github.com/mmalmsten/PlantWatcher) and refinements will come.

## 🐞 Prerequisites
Nothing should be running on port 8888, that's where we'll run the pigpio tcp server.

## 🚀 Getting started

Add the following to your `Makefile`

```
DEPS = pigpio
dep_pigpio = git https://github.com/mmalmsten/pigpio.git
```

Start pigpio tcp server on your Raspberry pi

```
sudo pigpiod
```

## 🛰 Usage

#### Start a genserver for the gpio pin

```
{ok, Pid} = pigpio:start_link(Gpio_pin)
```

### Examples:

#### Set pin mode to input
```
ok = pigpio:cast(Pid, {command, setmode, 0})
```

#### Read from the pin once and store in genserver state
```
ok = pigpio:cast(Pid, {read, once})
```

#### Read from the pin every N millisecond and store in genserver state
```
ok = pigpio:cast(Pid, {read, N})
```

#### Get the latest reading from pin

```
Reply = pigpio:call(Pid, read)
```

### Supported commands
- {read, Gpio}

- {getmode, Gpio}

- {setmode, Gpio, Mode} ➡️ Input = 0, Output = 1

- {write, Gpio, Level}

- {setpullupdown, Gpio, Pid} ➡️ Off = 0, Down = 1, Up (3.3v) = 2 - high/low


## TODO:
- [ ] Add possibility to add a listener to a pin and update current status in genserver state
