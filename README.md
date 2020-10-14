# ⚠️ WORK IN PROGRESS ⚠️

# Pigpio - Using Erlang for GPIO on a Raspberry Pi

Using Erlang for GPIO on a Raspberry Pi with http://abyz.me.uk/rpi/pigpio/

An OTP library

## 🚀 Getting started

Add the following to your `Makefile`

```
DEPS = pigpio
dep_pigpio = git https://github.com/mmalmsten/pigpio.git
```

## 🛰 Usage

#### Connect to a gpio pin

```
{ok, Pid} = pigpio:start_link("192.168.0.20", 8888, Gpio)
```

#### Read from gpio pin

```
Msg = pigpio:read(Pid)
```

#### Write to gpio pin

```
Msg = pigpio:write(Pid, Msg)
```
