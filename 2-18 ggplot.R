ggplot(data = mpg, aes(x = cty, y = hwy)) +
  geom_jitter() +
  theme_bw()

ggplot(data = mpg, aes(x = cty, y = hwy)) +
  geom_jitter() +
  theme_minimal()

ggplot(data = mpg, aes(x = cty, y = hwy, color = manufacturer)) +
  geom_jitter() +
  theme_bw()

ggplot(data = mpg, aes(x = cty, y = hwy)) +
  geom_hex() +
  theme_minimal()

ggplot(data = mpg, aes(x = cty, y = hwy)) +
  geom_hex() +
  theme_economist()
