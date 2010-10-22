# Require RubyGems by default.
require 'rubygems'


#class Object
#  def grep_methods(search_term)
#    methods.find_all {|method| method.downcase.include? search_term.downcase}
#  end
#end

require 'wirble'

# start wirble (with color)
begin
  Wirble.init
  Wirble.colorize
rescue LoadError => err
  warn "Couldn't load Wirble: #{err}"
end

#begin
#  require 'bond'
#  Bond.start
#end
