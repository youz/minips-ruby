
desc 'run tests'
task :test do
  sh 'ruby minips_test.rb'
end

desc 'start repl'
task :repl do
  sh 'ruby minips.rb -i'
end

desc 'run PostScript program'
task :run, 'filepath' do |t, args|
  sh 'ruby minips.rb ' + args['filepath']
end

task :default => :repl
