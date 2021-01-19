# Pigpio - Using Erlang for GPIO on a Raspberry Pi

Using Erlang for GPIO on a Raspberry Pi with [http://abyz.me.uk/rpi/pigpio/](http://abyz.me.uk/rpi/pigpio/)

⚠️ This library is still very much in a WORK IN PROGRESS state. I'm currently integrating it in [one of my pet projects](https://github.com/mmalmsten/PlantWatcher) and refinements will come.

Many thanks to [skvamme](https://github.com/skvamme) for letting me fork her implementation on accessing GPIO on a raspberry pi with pigpio and Erlang (and apologies for adding the genserver 😉).

## 🐞 Prerequisites

Nothing should be running on port 8888, that's where we'll run the pigpio tcp server.

## 🚀 Getting started

Add the following to your `Makefile`

```
DEPS = pigpio
dep_pigpio = git https://github.com/mmalmsten/pigpio.git
```

Install pigpio on your Raspberry pi

```
sudo apt-get install pigpio python-pigpio python3-pigpio
```

Start the pigpio daemon (tcp server)

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

#### Register an LED 💡

```
led(Gpio) ->
    {ok, Pid} = pigpio:start_link(Gpio),
    gen_server:cast(Pid, {command, setmode, 1}),
    {ok, Pid}.

{ok, Pid} = led(?GPIO_PIN),

pigpio:cast(Pid, {command, setpullupdown, 1}) % On

pigpio:cast(Pid, {command, setpullupdown, 1}) % Off
```

#### Register a button 🔘

```
button(Gpio) ->
    {ok, Pid} = pigpio:start_link(Gpio),
    pigpio:cast(Pid, {command, setmode, 0}),
    pigpio:cast(Pid, {command, setpullupdown, 2}),
    {ok, Pid}.

{ok, Pid} = button(?GPIO_PIN),

Status = pigpio:call(Pid, read),
```

## TODO:

- [ ] Add possibility to add a listener to a pin and update current status in genserver state
