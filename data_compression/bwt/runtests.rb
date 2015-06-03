#!/usr/bin/env ruby

require 'benchmark'

TESTS = "tests"

ExecutionRuntimeError = Class.new(StandardError)

def name_generator(name, count)
  count.times.map do |i|
    "#{name}#{i}.#{name}"
  end
end



TEST_CASES = {
  default: {
    tests: [
      'empty', 'onlychar', 'onechar', 'spacja', 'dwaslowa', 'jednozdanie', 'krotkie',
      '253', '254', '255', '256', '257', '258',
      '1022', '1023', '1024', '1025', '1026', '2048',
      'minipantadeusz',
      'pantadeusz', '1MB.bin',
    ],
    plans: [
      # { name: "lexi", size: 256 },
      # { name: "lexi", size: 1024 },
      { name: "uni", size: 256 },
      { name: "uni", size: 1024 },
      { name: "suffix", size: 256 },
      { name: "suffix", size: 1024 },
    ]
  },
  bin: {
    tests: name_generator("bin", 20),
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
  trzysta: {
    tests: name_generator("trzysta", 1000),
    plans: [ { name: "suffix", size: 256 } ]
  },
  mobi: {
    tests: name_generator("mobi", 20),
    plans: [
      { name: "uni", size: 100*1024 },
      { name: "uni", size: 200*1024 },
      { name: "uni", size: 300*1024 },
      { name: "uni", size: 400*1024 },
      { name: "uni", size: 500*1024 },
      { name: "uni", size: 600*1024 },
      { name: "uni", size: 700*1024 },
      { name: "uni", size: 800*1024 },
      { name: "uni", size: 900*1024 },
    ]
  },
  epub: {
    tests: name_generator("epub", 20),
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
  pdf: {
    tests: name_generator("pdf", 20),
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
  rfc: {
    tests: [
      # 'rfc1.txt',
      # 'rfc2.txt',
      # 'rfc3.txt', # nope
      # 'rfc4.txt', # nope
      # 'rfc5.txt', # nope
      # 'rfc6.txt', # nope
      # 'rfc7.txt', # nope
      'rfc8.txt',
      'rfc9.txt',
      'rfc10.txt',
      'rfc11.txt',
      'rfc12.txt',
      'rfc13.txt',
      'rfc14.txt',
    ],
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

  if ARGV[0].nil?
    run_test_cases("default")
  else
    run_test_cases(ARGV[0])
  end
end

def run_test_cases(testcase_name)
  testcase = TEST_CASES[testcase_name.to_sym]
  tests = testcase[:tests]
  plans = testcase[:plans]

  @timing_sum = 0

  print_first_header(plans)
  print_second_header(plans)
  tests.each do |name|
    path = File.join(TESTS, name)

    print name.ljust(COLUMN_SIZE[:header2_name])
    print File.size("#{path}.input").to_s.ljust(COLUMN_SIZE[:header2_size])

    plans.each do |test|
      begin
        case test[:name]
        when "lexi"
          compress_lexi(path, test[:size])
          decompress0(path, test[:size])
        when "uni"
          compress_uni(path, test[:size])
          decompress0(path, test[:size])
        when "suffix"
          compress_suffix(path, test[:size])
          decompress1(path, test[:size])
        else
          puts "Zły typ testu."
          exit(1)
        end
      rescue ExecutionRuntimeError
        puts
      end
    end

    puts
  end

  puts "Suma czasow: #{@timing_sum}"
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
  compile "compress-uni"
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
  @timing_sum += timing

  efficiency = calculate_efficiency(path)

  print calculate_time(timing).ljust(COLUMN_SIZE[:header2_comp])
  print efficiency.ljust(COLUMN_SIZE[:header2_eff])
end

def compress_uni(path, chunk_size = nil)
  timing = execute("./compress-uni #{path}.input #{path}.compressed #{chunk_size.to_s}")
  @timing_sum += timing

  efficiency = calculate_efficiency(path)

  print calculate_time(timing).ljust(COLUMN_SIZE[:header2_comp])
  print efficiency.ljust(COLUMN_SIZE[:header2_eff])
end

def compress_suffix(path, chunk_size = nil)
  timing = execute("./compress-suffix #{path}.input #{path}.compressed #{chunk_size.to_s}")
  @timing_sum += timing

  efficiency = calculate_efficiency(path)

  print calculate_time(timing).ljust(COLUMN_SIZE[:header2_comp])
  print efficiency.ljust(COLUMN_SIZE[:header2_eff])
end

def decompress0(path, chunk_size = nil)
  timing = execute("./decompress0 #{path}.compressed #{path}.decompressed #{chunk_size.to_s}")
  @timing_sum += timing

  execute("diff #{path}.input #{path}.decompressed")

  print calculate_time(timing).ljust(COLUMN_SIZE[:header2_decomp])
end

def decompress1(path, chunk_size = nil)
  timing = execute("./decompress1 #{path}.compressed #{path}.decompressed #{chunk_size.to_s}")
  @timing_sum += timing

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
