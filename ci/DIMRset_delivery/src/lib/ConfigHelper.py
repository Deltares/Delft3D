import configparser

class ConfigHelper():
    def __init__(self, config_file: str):
        self.config_file = config_file
        config = configparser.ConfigParser()
        config.read(config_file)
        self.config = config


class ConfigReader(ConfigHelper):
    def __init__(self, config_file: str):
        super().__init__(config_file)

    def get_section(self, section: str):
        return self.config[section]

    def get_section_key(self, section: str, key: str):
        return self.get_section(section)[key]


class ConfigWriter(ConfigHelper):
    def __init__(self, config_file: str):
        super().__init__(config_file)

    def set_section_key(self, section: str, key: str, value: str):
        self.config[section][key] = value

    def write(self):
        with open(self.config_file, 'w') as f:
            self.config.write(f)