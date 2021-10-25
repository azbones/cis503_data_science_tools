import setuptools

with open('README.md', 'r') as fh:
    long_description = fh.read()

setuptools.setup(
    name='CIS 503',
    version='0.0.1',
    description='This is for myBinder to run Python files',
    long_description=long_description,
    long_description_content_type='text/markdown',
    url='https://github.com/azbones/cis503_data_science_tools',
    packages=setuptools.find_packages(),
    install_requires=[
        'pandas',
        'matplotlib',
        'easytello'
    ],
    classifiers=[
        'Programming Language :: Python :: 3',
        'License :: OSI Approved :: MIT License',
        'Operating System :: OS Independent',
    ],
)
