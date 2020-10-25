# ⚠️ WORK IN PROGRESS ⚠️

# Pigpio - Using Erlang for GPIO on a Raspberry Pi

Using Erlang for GPIO on a Raspberry Pi with http://abyz.me.uk/rpi/pigpio/

## 🐞 Prerequisites

Nothing should be running on port 8888, that's where we'll run the pigpio tcp server.

## 🚀 Getting started

Add the following to your `Makefile`

```
DEPS = pigpio
dep_pigpio = git https://github.com/mmalmsten/pigpio.git
```

## 🛰 Usage

### Gpio mailbox

#### Connect to a gpio pin

```
{ok, Pid} = pigpio:start(Gpio, Type)
```

#### Read from gpio pin

```
Msg = pigpio:read(Gpio)
```

#### Write to gpio pin

```
Msg = pigpio:write(Gpio, Msg)
```

### 💡 Supported types

#### 🔘 Button

**Mode:** input
**Input:** high (3.3V)
**Read current status from pin:** every second
