from setuptools import setup

setup(
    name="lazy_json_validator",
    author="Trevor OConnor",
    version="0.0.1",
    packages=["lazy_json_validator"],
    package_dir={"lazy_json_validator": "lazy_json_validator"},
    include_package_data=True
    # package_data={"": ["py.typed"]},
)
