#!/usr/bin/env ruby

require 'benchmark'

TESTS = "tests"

SMALLER = [
  'empty',
  'onlychar',
  'onechar',
  'spacja',
  'dwaslowa',
  'jednozdanie',
  'krotkie',
  '253',
  '254',
  '255',
  '256',
  '257',
  '258',
  '1022',
  '1023',
  '1024',
  '1025',
  '1026',
  '2048',
  'minipantadeusz',
]

BIGGER = [
  'pantadeusz'
]

return unless system("make compress-lexi")
return unless system("make compress-suffix")
return unless system("make decompress0")
return unless system("make decompress1")

def compress_lexi(path, chunk_size = nil)
  result = nil
  timing = Benchmark.realtime do
    result = `./compress-lexi #{path}.input #{path}.compressed #{chunk_size.to_s}`
  end

  timing = (timing * 1000).to_i
  unless $?.success?
    puts "Runtime!"
    puts result
    exit(1)
  end

  compressed_file = File.size("#{path}.compressed")

  print timing.to_s.ljust(8)
  print compressed_file.to_s.ljust(8)
end

def compress_suffix(path, chunk_size = nil)
  result = nil
  timing = Benchmark.realtime do
    result = `./compress-suffix #{path}.input #{path}.compressed #{chunk_size.to_s}`
  end

  timing = (timing * 1000).to_i
  unless $?.success?
    puts "Runtime!"
    puts result
    exit(1)
  end

  compressed_file = File.size("#{path}.compressed")

  print timing.to_s.ljust(8)
  print compressed_file.to_s.ljust(8)
end

def decompress0(path, chunk_size = nil)
  result = nil
  timing = Benchmark.realtime do
    result = `./decompress0 #{path}.compressed #{path}.decompressed #{chunk_size.to_s}`
  end

  timing = (timing * 1000).to_i
  unless $?.success?
    puts "Runtime!"
    puts result
    exit(1)
  end

  result = `diff #{path}.input #{path}.decompressed`
  unless $?.success?
    puts "Diff!"
    puts result
    exit(1)
  end

  print timing.to_s.ljust(8)
end

def decompress1(path, chunk_size = nil)
  result = nil
  timing = Benchmark.realtime do
    result = `./decompress1 #{path}.compressed #{path}.decompressed #{chunk_size.to_s}`
  end

  timing = (timing * 1000).to_i
  unless $?.success?
    puts "Runtime!"
    puts result
    exit(1)
  end

  result = `diff #{path}.input #{path}.decompressed`
  unless $?.success?
    puts "Diff!"
    puts result
    exit(1)
  end

  print timing.to_s.ljust(8)
end

SMALLER.each do |name|
  path = File.join(TESTS, name)
  print "Wykonywanie #{name.ljust(16)}"

  compress_lexi(path)
  decompress0(path)
  compress_lexi(path, 1024)
  decompress0(path, 1024)
  compress_suffix(path)
  decompress1(path)

  puts
end

puts "\nWszystko OK!"
