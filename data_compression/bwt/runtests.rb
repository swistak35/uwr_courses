#!/usr/bin/env ruby

require 'benchmark'

TESTS = "tests"

TEST_CASES = {
  text: {
    tests: [
      'empty', 'onlychar', 'onechar', 'spacja', 'dwaslowa', 'jednozdanie', 'krotkie',
      '253', '254', '255', '256', '257', '258',
      '1022', '1023', '1024', '1025', '1026', '2048',
      'minipantadeusz',
    ],
    plans: [
      { name: "lexi", size: 256 },
      { name: "lexi", size: 1024 },
      { name: "suffix", size: 256 },
      { name: "suffix", size: 1024 },
    ]
  },
  bin: {
    tests: [ '1MB.bin', '5MB.bin', ],
    plans: [
      { name: "suffix", size: 100*1024 },
      { name: "suffix", size: 200*1024 },
      { name: "suffix", size: 300*1024 },
      { name: "suffix", size: 400*1024 },
      { name: "suffix", size: 500*1024 },
      { name: "suffix", size: 600*1024 },
      { name: "suffix", size: 700*1024 },
      { name: "suffix", size: 800*1024 },
      { name: "suffix", size: 900*1024 },
    ]
  },
}

COLUMN_SIZE = {
  header1_information: 32,
  header1_per_test:    20,
  header2_name:        16,
  header2_size:        16,
  header2_comp:        8,
  header2_eff:         4,
  header2_decomp:      8,
}

def main
  compile_all

  run_test_cases("bin")

  puts
  puts "Wszystko OK!"
end

def run_test_cases(testcase_name)
  testcase = TEST_CASES[testcase_name.to_sym]
  tests = testcase[:tests]
  plans = testcase[:plans]

  print_first_header(plans)
  print_second_header(plans)
  tests.each do |name|
    path = File.join(TESTS, name)

    print name.ljust(COLUMN_SIZE[:header2_name])
    print File.size("#{path}.input").to_s.ljust(COLUMN_SIZE[:header2_size])

    plans.each do |test|
      case test[:name]
      when "lexi"
        compress_lexi(path, test[:size])
        decompress0(path, test[:size])
      when "suffix"
        compress_suffix(path, test[:size])
        decompress1(path, test[:size])
      else
        puts "Zły typ testu."
        exit(1)
      end
    end

    puts
  end
end

def calculate_time(time_in_ms)
  if time_in_ms >= 1000
    seconds = (time_in_ms.to_f / 1000).round(1)
    "#{seconds}s"
  else
    "#{time_in_ms}ms"
  end
end

def compile(name)
  exit(1) unless system("make #{name}")
end

def compile_all
  compile "compress-lexi"
  compile "compress-suffix"
  compile "decompress0"
  compile "decompress1"
end

def execute(cmd)
  # puts "Executing '#{cmd}'"

  result = nil
  timing = Benchmark.realtime do
    result = `#{cmd}`
  end

  timing = (timing * 1000).to_i

  unless $?.success?
    puts "Runtime Error"
    puts cmd
    raise ExecutionRuntimeError
  end

  timing
end

def calculate_efficiency(path)
  input_filesize = File.size("#{path}.input")
  compressed_filesize = File.size("#{path}.compressed")
  if input_filesize == 0
    efficiency = 100
  else
    efficiency = ((compressed_filesize.to_f / input_filesize) * 100).to_i
  end

  if efficiency >= 1000
    "NO"
  else
    "#{efficiency}%"
  end
end

def compress_lexi(path, chunk_size = nil)
  timing = execute("./compress-lexi #{path}.input #{path}.compressed #{chunk_size.to_s}")

  efficiency = calculate_efficiency(path)

  print calculate_time(timing).ljust(COLUMN_SIZE[:header2_comp])
  print efficiency.ljust(COLUMN_SIZE[:header2_eff])
end

def compress_suffix(path, chunk_size = nil)
  timing = execute("./compress-suffix #{path}.input #{path}.compressed #{chunk_size.to_s}")

  efficiency = calculate_efficiency(path)

  print calculate_time(timing).ljust(COLUMN_SIZE[:header2_comp])
  print efficiency.ljust(COLUMN_SIZE[:header2_eff])
end

def decompress0(path, chunk_size = nil)
  timing = execute("./decompress0 #{path}.compressed #{path}.decompressed #{chunk_size.to_s}")

  execute("diff #{path}.input #{path}.decompressed")

  print calculate_time(timing).ljust(COLUMN_SIZE[:header2_decomp])
end

def decompress1(path, chunk_size = nil)
  timing = execute("./decompress1 #{path}.compressed #{path}.decompressed #{chunk_size.to_s}")

  execute("diff #{path}.input #{path}.decompressed")

  print calculate_time(timing).ljust(COLUMN_SIZE[:header2_decomp])
end


def print_first_header(plans)
  print "Informacje o teście".ljust(COLUMN_SIZE[:header1_information])

  plans.each do |test|
    print "#{test[:name].capitalize} (#{test[:size]})".ljust(COLUMN_SIZE[:header1_per_test])
  end

  puts
end

def print_second_header(plans)
  print "Nazwa".ljust(COLUMN_SIZE[:header2_name])
  print "Rozmiar".ljust(COLUMN_SIZE[:header2_size])

  plans.size.times do
    print "comp".ljust(COLUMN_SIZE[:header2_comp])
    print "eff".ljust(COLUMN_SIZE[:header2_eff])
    print "decomp".ljust(COLUMN_SIZE[:header2_decomp])
  end

  puts
end

main
